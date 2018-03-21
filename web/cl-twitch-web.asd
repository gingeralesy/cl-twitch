(in-package #:cl-user)

(asdf:defsystem #:cl-twitch-web
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :depends-on ((:interface :database)
               (:interface :user)
               (:interface :auth)
               (:interface :profile)
               :r-data-model
               :r-simple-admin
               :r-clip
               :cl-twitch
               :crypto-shortcuts)
  :components ((:file "package")
               (:file "conditions")
               (:file "config")
               (:file "oauth")
               (:file "admin")
               (:file "web")))
