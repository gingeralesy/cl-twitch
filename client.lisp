(in-package #:cl-twitch)

(defparameter *server-address* "irc.chat.twitch.tv")

(defvar *client* NIL)

(defun client ()
  *client*)


(defclass chat-client ()
  ((username :initarg :username :accessor username)
   (password :initarg :password :accessor password)
   (maiden :initform NIL :accessor maiden))
  (:default-initargs :username (error "USERNAME required")
                     :password (error "PASSWORD required")))

(defmethod initialize-instance :after ((client chat-client) &key (server *server-address*)
                                                                 (channel (error "CHANNEL required")))
  (let ((channel (if (char= #\# (char channel 0)) channel (format NIL "#~a" channel))))
    (setf (maiden client) (maiden:make-core
                           `(:maiden-irc :password ,(password client)
                                         :nickname ,(username client)
                                         :host ,server
                                         :channels (,channel)
                                         :capabilities (twitch.tv/tags
                                                        twitch.tv/commands
                                                        twitch.tv/membership))
                           :maiden-commands))))

(defun start (name password channel &optional (server *server-address*))
  (unless *client*
    (let ((client (make-instance 'chat-client :username name
                                              :password password
                                              :channel channel
                                              :server server)))
      (maiden:start client)
      (setf *client* client)
      client)))
