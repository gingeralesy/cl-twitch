(in-package #:cl-twitch-web)

(define-page index "twitchbot/$" (:clip "dashboard.ctml")
  (r-clip:process T :tokens-url (format NIL "https://id.twitch.tv/oauth2/authorize?client_id=~a&redirect_uri=~a&response_type=code&scope=~a"
                                        *client-id* *redirect-url* *scope*)))

(define-page oauth "twitchbot/oauth" ()
  (get-tokens (post/get "code"))
  (redirect (make-uri :domains '("twitchbot") :path "")))
