(defpackage #:mcclim-opengl
  (:use #:clim-lisp #:clim #:clime #:climb))
(in-package #:mcclim-opengl)

(defclass opengl-port (basic-port)
  ())

(defmethod find-port-type ((port (eql :opengl)))
  (values 'opengl-port (constantly nil)))

(defmethod invoke-with-output-to-drawing-stream
    (cont (backend opengl-port) (gl-context null) &key)
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :title "OpenGL Window" :flags '(:shown :opengl))
      (sdl2:delay 100)
      (sdl2:gl-set-attrs
       :context-major-version 3
       :context-minor-version 3
       :context-profile-mask sdl2-ffi:+sdl-gl-context-profile-core+)
      (sdl2:with-gl-context (context window)
        (sdl2:gl-make-current window gl-context)
        (gl:viewport 0 0 800 600)
        (gl:matrix-mode :projection)
        (gl:ortho -2 2 -2 2 -2 2)
        (gl:matrix-mode :modelview)
        (gl:load-identity)
        (gl:clear-color 0.0 0.0 1.0 1.0)
        (sdl2:with-event-loop ()
          (:idle ()
                 (gl:clear :color-buffer)
                 (funcall cont window)
                 (gl:flush)
                 (sdl2:gl-swap-window window))
          (:quit () t))))))

(defun make-gl-array (initial-contents)
  (let* ((len (length initial-contents))
         (arr (gl:alloc-gl-array :float len)))
    (dotimes (i len)
      (setf (gl:glaref arr i) (aref initial-contents i)))
    arr))

(defun do-it ()
  ;; Prepare data
  (let ((array (gl:gen-vertex-array))
        (vertex-buffer (gl:gen-buffer))
        (triangle (make-gl-array #(-1.0 -1.0 +0.0
                                   +1.0 -1.0 +0.0
                                   +0.0 +1.0 +0.0))))
    (gl:bind-vertex-array array)
    (gl:bind-buffer :array-buffer vertex-buffer)
    (gl:buffer-data :array-buffer :static-draw triangle)
    ;; draw
    (gl:enable-vertex-attrib-array 0)
    (gl:bind-buffer :array-buffer vertex-buffer)
    (gl:vertex-attrib-pointer 0 3 :float nil 0 0)
    (gl:draw-arrays :triangles 0 3)
    (gl:disable-vertex-attrib-array 0)
    (gl:free-gl-array triangle)
    (gl:flush)))
