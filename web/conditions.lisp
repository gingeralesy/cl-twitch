(in-package #:cl-twitch-web)

(define-condition cl-twitch-condition (radiance-condition)
  ())

(define-condition cl-twitch-warning (warning cl-twitch-condition)
  ())

(define-condition cl-twitch-error (error cl-twitch-condition)
  ())

(define-condition request-failed (cl-twitch-error)
  ((code :initarg :code :initform NIL :accessor code))
  (:report (lambda (con stream)
             (format stream "HTTP request failed: ~a~@[ ~a~]"
                     (code con) (message con)))))
