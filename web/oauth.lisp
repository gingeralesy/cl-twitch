(in-package #:cl-twitch-web)

(defparameter *token-storage* NIL)

(define-condition missing-data (cl-twitch-error)
  ())

(define-condition missing-token (cl-twitch-error)
  ((token :initarg :token :initform NIL :accessor token))
  (:report (lambda (con stream)
             (format stream "Missing token ~a" (token con)))))

(defclass token-storage ()
  ((data :initarg :data :reader data)))

(defmethod filled-p ((storage token-storage))
  (dm:hull-p (data storage)))

(defmethod save ((storage token-storage))
  (dm:save (data storage)))

(defmethod access-token ((storage token-storage))
  (dm:field (data storage) 'access-token))

(defmethod (setf access-token) (value (storage token-storage))
  (declare (type string value))
  (setf (dm:field (data storage) 'access-token) value))

(defmethod refresh-token ((storage token-storage))
  (dm:field (data storage) 'refresh-token))

(defmethod (setf refresh-token) (value (storage token-storage))
  (declare (type string value))
  (setf (dm:field (data storage) 'refresh-token) value))

(defmethod expire-time ((storage token-storage))
  (dm:field (data storage) 'expire-time))

(defmethod (setf expire-time) (value (storage token-storage))
  (setf (dm:field (data storage) 'expire-time) value))

(defmethod scope ((storage token-storage))
  (dm:field (data storage) 'scope))

(defmethod (setf scope) (value (storage token-storage))
  (setf (dm:field (data storage) 'scope) value))

(define-trigger db:connected ()
  (db:create 'token-storage '((access-token (:varchar 32))
                              (refresh-token (:varchar 32))
                              (expire-time (:integer 5))
                              (scope (:varchar 64))))
  (let ((data (or (dm:get-one 'token-storage (db:query :all))
                  (dm:hull 'token-storage))))
    (when (dm:hull-p data) (dm:insert data))
    (setf *token-storage* (make-instance 'token-storage :data data))))

(defun token-expired-p (&optional (storage *token-storage*))
  (declare (type token-storage storage))
  (not (and (not (filled-p storage))
            (expire-time storage)
            (< (expire-time storage) (get-universal-time)))))

(defun get-tokens (authorization-code)
  (declare (type string authorization-code))
  (multiple-value-bind (stream code headers _ __ close-p message)
      (drakma:http-request "https://id.twitch.tv/oauth2/token"
                           :parameters `(("client_id" . ,(client-id))
                                         ("client_secret" . ,(client-secret))
                                         ("code" . ,authorization-code)
                                         ("grant_type" . "authorization_code")
                                         ("redirect_uri" . ,(redirect-url))
                                         ("scope" . ,(scope-config)))
                           :method :post
                           :want-stream T)
    (declare (ignore headers _ __))
    (unwind-protect
         (if (< code 400)
             (update-tokens stream)
             (error 'request-failed :code code :message message))
      (when close-p (ignore-errors (close stream))))))

(defun refresh-tokens (&optional (storage *token-storage*))
  (when (token-expired-p)
    (let ((refresh-token (or* (refresh-token storage)
                              (error 'missing-token :token 'refresh-token)))
          (scope (or* (scope storage) (scope-config))))
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
     (access-token storage) (or* (gethash "access_token" data)
                                 (access-token storage))
     (refresh-token storage) (or* (gethash "refresh_token" data)
                                  (refresh-token storage))
     (expire-time storage) (if (gethash "expires_in" data)
                               (+ (gethash "expires_in" data)
                                  (get-universal-time))
                               (expire-time storage))
     (scope storage) (or* (gethash "scope" data)
                          (scope storage)))
    (save storage)))
