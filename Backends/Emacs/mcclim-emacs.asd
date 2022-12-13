(in-package #:asdf-user)

(defsystem "mcclim-emacs"
  :depends-on (
               "mcclim-svg")
  :serial t
  :components ((:file "emacs")))

