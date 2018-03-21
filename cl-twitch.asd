(in-package #:cl-user)

(asdf:defsystem #:cl-twitch
  :depends-on (:alexandria
               :flexi-streams
               :drakma
               :yason
               :maiden
               :maiden-irc
               :maiden-commands
               :for
               :verbose)
  :components ((:file "package")
               (:file "client")))
