(in-package #:mcclim-sdl2)

(defmethod realize-mirror ((port sdl2-port) (sheet unmanaged-sheet-mixin))
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (create-window "(McCLIM)" x y w h '(:borderless) :synchronize t)))

(defmethod realize-mirror ((port sdl2-port) (sheet top-level-sheet-mixin))
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (let* ((title (sheet-pretty-name sheet))
           (window-id (create-window title x y w h '(:shown :resizable)
                                     :synchronize t)))
      (alx:when-let ((icon (sheet-icon sheet)))
        (change-window-icon window-id (alx:ensure-car icon)))
      (set-mirror-sheet port window-id sheet)
      window-id)))

#+ (or) ;; SDL2 port does not implement mirrored sub-windows.
(defmethod realize-mirror ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (let ((window-id (create-window "McCLIM" x y w h '(:shown :resizable)
                                    :synchronize t)))
      (set-mirror-sheet port window-id sheet)
      window-id)))

(defmethod destroy-mirror ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (let ((window-id (sheet-direct-mirror sheet)))
    (destroy-window window-id)
    (set-mirror-sheet port window-id nil)))

(defmethod port-set-mirror-geometry (port (sheet mirrored-sheet-mixin) region)
  (with-bounding-rectangle* (x1 y1 x2 y2 :width w :height h) region
    (change-window-size (sheet-direct-mirror sheet) x1 y1 w h)
    (values x1 y1 x2 y2)))

(defmethod port-enable-sheet ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (show-window (sheet-direct-mirror sheet)))

(defmethod port-disable-sheet ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (hide-window (sheet-direct-mirror sheet)))

;;; The following functions are specific to top-level sheets.

(defmethod port-set-mirror-name
    ((port sdl2-port) (sheet top-level-sheet-mixin) name)
  (change-window-title (sheet-direct-mirror sheet) name))

(defmethod port-set-mirror-icon
    ((port sdl2-port) (sheet top-level-sheet-mixin) icon)
  (alx:when-let ((window (sheet-direct-mirror sheet)))
    (change-window-icon (sheet-direct-mirror sheet) icon)))

(defmethod port-shrink-sheet ((port sdl2-port) (sheet top-level-sheet-mixin))
  (minimize-window (sheet-direct-mirror sheet)))

(defmethod port-unshrink-sheet ((port sdl2-port) (sheet top-level-sheet-mixin))
  (restore-window (sheet-direct-mirror sheet)))

(defmethod raise-mirror ((port sdl2-port) (sheet top-level-sheet-mixin))
  (raise-window (sheet-direct-mirror sheet)))

(defmethod bury-mirror ((port sdl2-port) (sheet top-level-sheet-mixin))
  (log:warn "Unsupported operation."))


;;; Requests

(define-sdl2-request create-window (title x y width height flags)
  (log:info "Creating a new window.")
  (let ((window (sdl2:create-window :title title :flags flags
                                    :x x :y y :w width :h height)))
    (sdl2-ffi.functions:sdl-get-window-id window)))

(define-sdl2-request destroy-window (window-id)
  (log:info "Destroying window ~s." window-id)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2:destroy-window window)))

(define-sdl2-request change-window-title (window-id title)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-set-window-title window title)))

(define-sdl2-request change-window-icon (window-id icon)
  (alx:when-let ((window (sdl2-window window-id)))
    (let* ((array (pattern-array icon))
           (width (pattern-width icon))
           (height (pattern-height icon))
           (depth 32)
           (pitch (* 4 width)))
      (cffi:with-foreign-array (arr array `(:array :uint32 ,height ,width))
        (let ((surface
                (sdl2-ffi.functions:sdl-create-rgb-surface-from
                 arr width height depth pitch
                 #x00ff0000 #x0000ff00 #x000000ff #xff000000)))
          (unwind-protect
               (sdl2-ffi.functions:sdl-set-window-icon window surface)
            (sdl2-ffi.functions:sdl-free-surface surface)))))))

(define-sdl2-request change-window-size (window-id x y w h)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-set-window-position window x y)
    (sdl2-ffi.functions:sdl-set-window-size window w h)))

(define-sdl2-request hide-window (window-id)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-hide-window window)))

(define-sdl2-request show-window (window-id)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-show-window window)))

(define-sdl2-request minimize-window (window-id)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-minimize-window window)))

(define-sdl2-request restore-window (window-id)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-restore-window window)))

;;; Specified by SDL2 but doesn't seem to work on X11.
(define-sdl2-request raise-window (window-id)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-raise-window window)))

#+ (or) ;; Not specified by SDL2.
(define-sdl2-request bury-window (window-id)
  (alx:when-let ((window (sdl2-window window-id)))
    (sdl2-ffi.functions:sdl-bury-window window)))


;;; Window SDL2 event handlers.

(define-sdl2-handler (ev :windowevent) (event window-id timestamp data1 data2)
  (alx:when-let ((sheet (get-mirror-sheet *sdl2-port* window-id)))
    (let ((event-key (autowrap:enum-key '(:enum (windowevent.event)) event)))
      (handle-sdl2-window-event event-key sheet timestamp data1 data2))))

(defgeneric handle-sdl2-window-event (event-key sheet timestamp data1 data2)
  (:method (event-key sheet timestamp data1 data2)
    (log:debug "Unhandled window event ~s." event-key)))

(defmethod handle-sdl2-window-event ((key (eql :close)) sheet stamp d1 d2)
  (log:info "Destroying a window.")
  (make-instance 'window-manager-delete-event :sheet sheet :timestamp stamp))

;;; Between pressing quit and the actual close the user may still use the
;;; window for a brief period, so i.e a window event may sneak in. The window
;;; event handler should ignore events to windows that are already destroyed.
(defmethod handle-sdl2-window-event ((key (eql :exposed)) sheet stamp d1 d2)
  (log:info "Repainting a window.")
  ;; The call to GET-WINDOW-SURFACE is for side the effect, namely to ensure
  ;; that the surface is allocated (to be able to call UPDATE-WINDOW).
  (let ((window (sdl2-window (sheet-mirror sheet))))
    (sdl2:get-window-surface window)
    (sdl2:update-window window)))
