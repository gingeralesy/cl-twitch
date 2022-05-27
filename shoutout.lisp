(in-package #:cl-twitch)

(defvar *shoutouts* (make-hash-table :test #'equal))

(defvar *shoutouts-done* (make-hash-table :test #'equal))

(maiden:define-consumer shoutout (maiden:agent)
  ())

(defun handle-shoutout (ev user)
  (let* ((name (if (stringp user) user (maiden:name user)))
         (key (format NIL "~(~a~)" name))
         (last (gethash key *shoutouts-done*))
         (message (gethash key *shoutouts*)))
    (when (and message
               (or (null last)
                   (local-time:timestamp<
                    (local-time:timestamp+ last 90 :minute)
                    (local-time:now))))
      (setf (gethash key *shoutouts-done*) (local-time:now))
      (maiden-client-entities:reply
       ev (format NIL "Please go see our lovely friend @~a at https://www.twitch.tv/~a~@[ ~a~]"
                  name key (when (stringp message) message))))))

(maiden:define-handler (shoutout new-message maiden-irc:reply-event) (c ev user)
  (v:info :shoutout "Message received: ~a" ev)
  (handle-shoutout ev user))

(defun add-shoutout (user &optional message)
  ;; TODO: Also add this to the database.
  (let ((key (format NIL "~(~a~)" (if (stringp user) user (maiden:name user)))))
    (setf (gethash key *shoutouts*) (or message :empty))))

(defun remove-shoutout (user)
  ;; TODO: Also remove this from the database.
  (let ((key (format NIL "~(~a~)" (if (stringp user) user (maiden:name user)))))
    (setf (gethash key *shoutouts*) NIL)))

(defun init-shoutouts ()
  ;; TODO: Get these from the database.
  (add-shoutout "Shinmera")
  (add-shoutout "TheAgentKP")
  (add-shoutout "Doctor1297"))
