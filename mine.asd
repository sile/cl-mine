(in-package :asdf)

(defsystem mine
  :name "mine"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "A console based mine-sweeper"
  
  :serial t
  :components ((:file "package")
               (:file "console")
               (:file "game")
               (:file "mine")))
