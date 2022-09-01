(in-package #:mcclim-opengl)

(defun gl-init-scene ()
  (sdl2:gl-set-attrs
   :context-major-version 3
   :context-minor-version 3
   :context-profile-mask sdl2-ffi:+sdl-gl-context-profile-core+)
  
  (gl:viewport 0 0 800 600)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:clear-color 0.0 0.0 1.0 1.0)
  (gl:clear :color-buffer))

(defun gl-draw-scene ()
  (gl:clear :color-buffer)
  (gl:begin :triangles)
  (gl:color 1.0 0.0 0.0)
  (gl:vertex 0.0 1.0)
  (gl:vertex -1.0 -1.0)
  (gl:vertex 1.0 -1.0)
  (gl:end)
  (gl:flush))

(defun sdl2-draw-frame (context window)
  (sdl2:gl-make-current window context)
  (gl-draw-scene)
  (sdl2:gl-swap-window window))

(defun basic-test ()
  (sdl2:with-init (:video)
    (sdl2:with-window (window :flags '(:shown :opengl))
      (sdl2:with-gl-context (context window)
        ;; GOTCHA some window managers (most notably kwin) return the window
        ;; before it is available on the screen. If we swap the window in the
        ;; meantime then it won't be visible. We need to add a kludge (delay)
        ;; or wait for the window exposure event.
        (gl-init-scene)
        (sdl2-draw-frame context window)
        (sdl2:with-sdl-event (event)
          (loop for rc = (sdl2:next-event event :wait)
                for event-type = (sdl2:get-event-type event) do
                  (case event-type
                    (:quit
                     (return))
                    (:windowevent
                     (sdl2-draw-frame context window)))))))))

