(in-package #:cl-user)

(asdf:defsystem #:cl-twitch
  :depends-on (:alexandria
               :flexi-streams
               :drakma
               :yason
               :sqlite
               :maiden
               :maiden-irc
               :maiden-commands
               :trivial-features
               :for
               :verbose
               :local-time)
  :components ((:file "package")
               (:file "db")
               (:file "client")))
