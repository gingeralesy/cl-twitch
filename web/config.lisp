(in-package #:cl-twitch-web)

;; TODO: Store these in Radiance config
(defparameter *client-id* "bqyhli5uhnrgk243a5h34uk7qxkwwy")
(defparameter *client-secret* "o5ijqz93w22ven6vz5k60007khy7zt")
(defparameter *redirect-url* "http://localhost:8080/!/twitchbot/oauth")
(defparameter *scope* "channel_editor")

(define-condition not-configured (cl-twitch-error)
  ((config-name :initarg :config :initform NIL :accessor config-name))
  (:report (lambda (con stream)
             (format stream "CL-TWITCH-WEB has not been configured properly. Missing ~a.~%"
                     (config-name con)))))

(defun client-id ()
  (if *client-id* *client-id* (error 'not-configured :config 'client-id)))

(defun (setf client-id) (value)
  (setf *client-id* value))

(defun client-secret ()
  (if *client-secret* *client-secret* (error 'not-configured :config 'client-secret)))

(defun (setf client-secret) (value)
  (setf *client-secret* value))

(defun redirect-url ()
  (if *redirect-url* *redirect-url* (error 'not-configured :config 'redirect-url)))

(defun (setf redirect-url) (value)
  (setf *redirect-url* value))

(defun scope-config ()
  (if *scope* *scope* (error 'not-configured :config 'scope)))

(defun (setf scope-config) (value)
  (setf *scope* value))
