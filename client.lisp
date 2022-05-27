(in-package #:cl-twitch)

(defparameter *server-address* "irc.chat.twitch.tv")

(defparameter *redirect-uri* "http://localhost:8080/!/twitchbot/oauth")

(defparameter *client-name* "twitch-tv")

(defparameter *client-scopes* '("chat:read" "chat:edit"))

(defparameter *client* NIL)

(defun authorize-url (&key (client-id *client-id*)
                           (redirect-uri *redirect-uri*)
                           (scope *client-scopes*)
                           (encode T))
  (let ((scope (etypecase scope
                 (string scope)
                 (list (format NIL "~{~a~^ ~}" scope)))))
    (format NIL "https://id.twitch.tv/oauth2/authorize?response_type=code&client_id=~a&redirect_uri=~a&scope=~a"
            (if encode (drakma:url-encode client-id :utf-8) client-id)
            (if encode (drakma:url-encode redirect-uri :utf-8) redirect-uri)
            (if encode (drakma:url-encode scope :utf-8) scope))))

(defclass chat-client ()
  ((last-refresh :initform NIL :accessor last-refresh)
   (access-token :initform NIL :accessor access-token)
   (refresh-token :initform NIL :accessor refresh-token)
   (expires-in :initform NIL :accessor expires-in)
   (core :initform NIL :accessor core)))

(defmethod store-client ((client chat-client))
  (store-client-fields
   (local-time:timestamp-to-unix (last-refresh client)) (access-token client) (refresh-token client) (expires-in client)
   *client-id*))

(defmethod initialize-instance :after ((client chat-client)
                                       &key (nickname *nickname*)
                                            (server *server-address*)
                                            (channel (error "CHANNEL required"))
                                            authorization-code)
  (if authorization-code
      (update-tokens client authorization-code)
      (let ((fields (load-client-fields *client-id*)))
        (unless fields (error "Could not load chat client."))
        (setf (last-refresh client) (local-time:unix-to-timestamp (getf fields :last-refresh)))
        (setf (access-token client) (getf fields :access-token))
        (setf (refresh-token client) (getf fields :refresh-token))
        (setf (expires-in client) (getf fields :expires-in))
        (when (token-expired-p client 5)
          (update-tokens client))))
  (let ((channel (if (char= #\# (char channel 0)) channel (format NIL "#~a" channel))))
    (setf (core client) (maiden:start (make-instance 'maiden:core)))
    (maiden:add-to-core
     (core client)
     `(maiden-irc:irc-client
       :name ,*client-name*
       :nickname ,nickname
       :password ,(format NIL "oauth:~a" (access-token client))
       :host ,server
       :channels (,channel))
     'maiden-commands:commands)))

(defmethod update-tokens ((client chat-client) &optional authorization-code)
  (unless (or authorization-code (refresh-token client))
    (error "Either Authorization Code or Refresh Token is required!"))
  (let* ((time (local-time:now))
         (parameters
           (nconc (list (cons "client_id" *client-id*) (cons "client_secret" *client-secret*))
                  (if authorization-code
                      (list (cons "grant_type" "authorization_code")
                            (cons "redirect_uri" *redirect-uri*)
                            (cons "code" authorization-code))
                      (list (cons "grant_type" "refresh_token")
                            (cons "refresh_token" (refresh-token client))))))
         (response (or (flexi-streams:octets-to-string
                        (drakma:http-request
                         "https://id.twitch.tv/oauth2/token"
                         :method :post
                         :parameters parameters))
                       (error "No response acquired.")))
         (json (yason:parse response))
         (status (gethash "status" json)))
    (when (and status (<= 400 status))
      (error "Request failed: ~a" (gethash "message" json)))
    (setf (last-refresh client) time)
    (setf (access-token client) (gethash "access_token" json))
    (setf (refresh-token client) (gethash "refresh_token" json))
    (setf (expires-in client) (gethash "expires_in" json))
    (store-client client)
    (v:info :client "Tokens updated.")))

(defmethod token-expires ((client chat-client))
  (when (and (expires-in client) (last-refresh client))
    (local-time:timestamp+ (last-refresh client) (expires-in client) :sec)))

(defmethod token-expired-p ((client chat-client) &optional offset-minutes)
  (let ((expire-time (token-expires client)))
    (when offset-minutes
      (setf expire-time (local-time:timestamp- expire-time offset-minutes :minute)))
    (or (null expire-time) (local-time:timestamp<= expire-time (local-time:now)))))

(defun start (code &key (server *server-address*) (channel "gingeralesy"))
  (unless *client*
    (setf *client* (make-instance 'chat-client :authorization-code code
                                               :server server
                                               :channel channel)))
  *client*)

(defun stop ()
  (when *client*
    (let ((client *client*))
      (setf *client* NIL)
      (maiden:stop (core client)))))

(defun server (&optional (client *client*) (name *client-name*))
  (when (token-expired-p client 5) (update-tokens client))
  (when client (maiden:consumer name (core client))))
