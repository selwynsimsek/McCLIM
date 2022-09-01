(in-package #:asdf-user)

(defsystem "mcclim-opengl"
  :description "OpenGL backend for McCLIM"
  :author "Daniel Kochmański"
  :depends-on (#:mcclim #:cl-opengl #:sdl2)
  :components ((:file "mcclim-opengl")))
