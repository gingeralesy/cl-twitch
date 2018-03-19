(in-package #:cl-user)

(asdf:defsystem #:cl-twitch-web
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :depends-on ((:interface :database)
               :cl-twitch
               :radiance
               :r-data-model
               :r-clip)
  :components ((:file "package")
               (:file "config")
               (:file "oauth")
               (:file "web")))
