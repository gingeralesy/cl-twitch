(in-package #:cl-twitch-web)

(defparameter *token-storage* NIL)

(define-condition missing-data (cl-twitch-error)
  ())

(define-condition missing-token (cl-twitch-error)
  ((token :initarg :token :initform NIL :accessor token))
  (:report (lambda (con stream)
             (format stream "Missing token ~a" (token con)))))

(define-trigger db:connected ()
  (db:create 'token-storage '((access-token (:varchar 32))
                              (refresh-token (:varchar 32))
                              (expire-time (:integer 5))
                              (scope (:varchar 64))))
  (let ((old-data (or (dm:get-one 'token-storage (db:query :all))
                      (dm:hull 'token-storage))))
    (when (dm:hull-p old-data)
      (dm:insert old-data))
    (setf *token-storage* old-data)))

(defun token-expired-p (&optional (storage *token-storage*))
  (declare (type dm:data-model storage))
  (not (and (not (dm:hull-p *token-storage*))
            (dm:field storage 'expire-time)
            (< (dm:field storage 'expire-time) (get-universal-time)))))

(defun get-tokens (authorization-code)
  (declare (type string authorization-code))
  (multiple-value-bind (stream code headers _ __ close-p message)
      (drakma:http-request "https://id.twitch.tv/oauth2/token"
                           :parameters `(("client_id" . ,(client-id))
                                         ("client_secret" . ,(client-secret))
                                         ("code" . ,authorization-code)
                                         ("grant_type" . "authorization_code")
                                         ("redirect_uri" . ,(redirect-url))
                                         ("scope" . ,(scope)))
                           :method :post
                           :want-stream T)
    (declare (ignore headers _ __))
    (unwind-protect
         (if (< code 400)
             (update-tokens stream)
             (error 'request-failed :code code :message message))
      (when close-p (ignore-errors (close stream))))))

(defun refresh-tokens ()
  (when (token-expired-p)
    (let ((refresh-token (or* (dm:field *token-storage* 'refresh-token)
                              (error 'missing-token :token 'refresh-token)))
          (scope (or* (dm:field *token-storage* 'scope) (scope))))
      (multiple-value-bind (stream code headers _ __ close-p message)
          (drakma:http-request "https://id.twitch.tv/oauth2/token"
                               :parameters `(("client_id" . ,(client-id))
                                             ("client_secret" . ,(client-secret))
                                             ("grant_type" . "refresh_token")
                                             ("refresh_token" . ,refresh-token)
                                             ("scope" . ,scope))
                               :method :post
                               :want-stream T)
        (declare (ignore headers _ __))
        (unwind-protect
             (if (< code 400)
                 (update-tokens stream)
                 (error 'request-failed :code code :message message))
          (when close-p (ignore-errors (close stream))))))))

(defun update-tokens (data)
  (unless data (error 'missing-data :message "DATA required"))
  (let ((data (etypecase data
                (hash-table data)
                (flexi-streams:flexi-io-stream
                 (setf (flexi-streams:flexi-stream-external-format data) :utf-8)
                 (yason:parse data))))
        (storage *token-storage*))
    (setf
     (dm:field storage 'access-token) (or* (gethash "access_token" data)
                                           (dm:field storage 'access-token))
     (dm:field storage 'refresh-token) (or* (gethash "refresh_token" data)
                                            (dm:field storage 'refresh-token))
     (dm:field storage 'expire-time) (if (gethash "expires_in" data)
                                         (+ (gethash "expires_in" data)
                                            (get-universal-time))
                                         (dm:field storage 'expire-time))
     (dm:field storage 'scope) (or* (gethash "scope" data)
                                    (dm:field storage 'scope)))
    (dm:save storage)))

