(in-package #:cl-twitch-web)

(radiance:define-implement-trigger admin
    (admin:define-panel settings cl-twitch (:clip "admin-panel.ctml")
      (with-actions (error info)
          ((:save
            (setf (client-id) (or* (post/get "client-id") (client-id))
                  (client-secret) (or* (post/get "client-secret") (client-secret))
                  (redirect-url) (or* (post/get "redirect-url") (redirect-url))
                  (scope-config) (or* (post/get "scope") (scope-config)))))
        (r-clip:process T :client-id (client-id)
                          :client-secret (client-secret)
                          :redirect-url (redirect-url)
                          :scope (scope-config)
                          :error error :info info))))
