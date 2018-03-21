(in-package #:cl-twitch-web)

(unless (boundp (find-symbol "+AUTH-URI-FORMAT+")) ;; FIXME: Why does this error otherwise?
  (defconstant +auth-uri-format+ "https://id.twitch.tv/oauth2/authorize?client_id=~a&redirect_uri=~a&response_type=code&scope=~a"))

(define-page index "twitchbot/$" (:clip "dashboard.ctml")
  (r-clip:process
   T
   :tokens-url (format NIL +auth-uri-format+ (client-id) (redirect-url) (scope))))

(define-page oauth "twitchbot/oauth" ()
  (get-tokens (post/get "code"))
  (redirect (make-uri :domains '("twitchbot") :path "")))
