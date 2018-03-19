(in-package #:cl-twitch-web)

(defparameter *token-storage* NIL)

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
  (let ((client-id *client-id*)
        (client-secret *client-secret*)
        (redirect-uri *redirect-url*)
        (scope *scope*))
    (update-tokens
     (drakma:http-request "https://id.twitch.tv/oauth2/token"
                          :parameters `(("client_id" . ,client-id)
                                        ("client_secret" . ,client-secret)
                                        ("code" . ,authorization-code)
                                        ("grant_type" . "authorization_code")
                                        ("redirect_uri" . ,redirect-uri)
                                        ("scope" . ,scope))
                          :method :post
                          :want-stream T))))

(defun refresh-tokens ()
  (when (token-expired-p)
    (let ((client-id *client-id*)
          (client-secret *client-secret*)
          (refresh-token (dm:field *token-storage* 'refresh-token))
          (scope (dm:field *token-storage* 'scope)))
      (update-tokens
       (drakma:http-request "https://id.twitch.tv/oauth2/token"
                            :parameters `(("client_id" . ,client-id)
                                          ("client_secret" . ,client-secret)
                                          ("grant_type" . "refresh_token")
                                          ("refresh_token" . ,refresh-token)
                                          ("scope" . ,scope))
                            :method :post
                            :want-stream T)))))

(defun update-tokens (data)
  (unless data (error "DATA required"))
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

