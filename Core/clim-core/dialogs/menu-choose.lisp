;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2002 by Alexey Dejneka (adejneka@comail.ru)
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Implementation of the MENU-CHOOSE function and surrounding
;;; machinery.
;;;

;;; Long time TODO (if someone wants to implement them - you are welcome):
;;;
;;; - Menu item options: :items, :type.
;;;
;;; - VIEW.
;;;
;;; - Caching.
;;;
;;; - Default item.

;;; Mid time TODO:
;;;
;;; - Documentation.
;;;
;;; - Empty menu.
;;;
;;; - :DIVIDER type menu items.

(in-package #:clim-internals)

;; Spec function.
(defgeneric menu-choose
    (items
     &key associated-window printer presentation-type default-item
     text-style label cache unique-id id-test cache-value cache-test
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     cell-align-x cell-align-y scroll-bars pointer-documentation))

;; Spec function.
(defgeneric frame-manager-menu-choose
    (frame-manager items
     &key associated-window printer presentation-type default-item
     text-style label cache unique-id id-test cache-value cache-test
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     cell-align-x cell-align-y scroll-bars pointer-documentation))

;; Spec function.
(defgeneric menu-choose-from-drawer
    (menu presentation-type drawer
     &key x-position y-position cache unique-id id-test cache-value cache-test
     default-presentation pointer-documentation))

(defgeneric adjust-menu-size-and-position (menu &key x-position y-position)
  (:documentation "Adjust the size of the menu so it fits
  properly on the screen with regards to the menu entries. `menu'
  should be the menu pane. This is an internal,
  non-specification-defined function."))

(defun menu-item-value (menu-item)
  (cond ((atom menu-item)
         menu-item)
        ((atom (cdr menu-item))
         (cdr menu-item))
        (t (getf (cdr menu-item) :value (car menu-item)))))

(defun menu-item-display (menu-item)
  (if (atom menu-item)
      menu-item
      (car menu-item)))

(defun menu-item-options (menu-item)
  (if (and (consp menu-item)
           (consp (cdr menu-item)))
      (cdr menu-item) ; XXX Remove :VALUE?
      nil))

(defun menu-item-option (menu-item option &optional default)
  (if (listp menu-item)
      (getf (menu-item-options menu-item) option default)
      default))

(defun print-menu-item (menu-item &optional (stream *standard-output*))
  (let ((style (getf (menu-item-options menu-item) :style '(nil nil nil))))
    (with-text-style (stream style)
      (if (menu-item-option menu-item :active t)
          (princ (menu-item-display menu-item) stream)
          (with-drawing-options (stream :ink (compose-over
                                                (compose-in
                                                  ; XXX it should be (MEDIUM-INK),
                                                  ; but CLX backend is too stupid.
                                                  ; -- APD, 2002-08-07
                                                  (medium-foreground stream)
                                                  (make-opacity 0.5))
                                                (medium-background stream)))
            (princ (menu-item-display menu-item) stream))))))

;; Spec function.
(defun draw-standard-menu
    (stream presentation-type items default-item
     &key item-printer
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     cell-align-x cell-align-y)
  (declare (ignore default-item))
  (orf item-printer #'print-menu-item)
  (format-items items
                :stream stream
                :printer
                (lambda (item stream)
                  (ecase (menu-item-option item :type :item)
                    (:item
                     ;; This is a normal item, just output.
                     (let ((activep (menu-item-option item :active t)))
                       (with-presentation-type-decoded (name params options)
                           presentation-type
                         (let ((*allow-sensitive-inferiors* activep))
                           (with-text-style
                               (stream (menu-item-option
                                        item :style
                                        '(:sans-serif nil nil)))
                             (with-output-as-presentation
                                 (stream
                                  item
                                  `((,name ,@params)
                                    :description ,(getf (menu-item-options item) :documentation)
                                    ,@options)
                                  :single-box t)
                               (funcall item-printer item stream)))))))
                    (:label
                     ;; This is a static label, it should not be
                     ;; mouse-sensitive, but not grayed out either.
                     (with-text-style (stream (menu-item-option
                                               item :style
                                               '(:sans-serif nil nil)))
                       (funcall item-printer item stream)))
                    (:divider
                     ;; FIXME: Should draw a line instead.
                     (with-text-style (stream (menu-item-option
                                               item :style
                                               '(:sans-serif :italic nil)))
                       (funcall item-printer item stream)))))
                :presentation-type nil
                :x-spacing x-spacing
                :y-spacing y-spacing
                :n-columns n-columns
                :n-rows n-rows
                :max-width max-width
                :max-height max-height
                :cell-align-x cell-align-x
                :cell-align-y (or cell-align-y :top)
                :row-wise row-wise))

(defclass menu-pane (clim-stream-pane)
  ((menu-frame))
  (:default-initargs :background *3d-normal-color*))


;; When the menu frame is created it is disabled.
;; To make the menu visible it is required to call enable-menu.
(defgeneric enable-menu (pane))

(defmethod enable-menu ((pane menu-pane))
  (enable-frame (slot-value pane 'menu-frame)))

;; Spec macro.
;; The menu is not visible.
(defmacro with-menu ((menu &optional associated-window
                           &key (deexpose t) label scroll-bars)
                     &body body)
  (check-type menu symbol)
  (with-gensyms (with-menu-cont)
    `(flet ((,with-menu-cont (,menu)
              ,@body))
       (declare (dynamic-extent #',with-menu-cont))
       (invoke-with-menu #',with-menu-cont
                         ,associated-window ; XXX
                         ',deexpose         ; XXX!!!
                         ,label
                         ,scroll-bars))))

(defun invoke-with-menu (continuation associated-window deexpose
                         label scroll-bars)
  (let* ((associated-frame (if associated-window
                               (pane-frame associated-window)
                               *application-frame*))
         (fm (frame-manager associated-frame)))
    (with-look-and-feel-realization (fm associated-frame) ; hmm... checkme
      (let* ((menu-stream (make-pane-1 fm associated-frame 'menu-pane))
             (container (scrolling (:scroll-bar scroll-bars)
                          menu-stream))
             (frame (make-menu-frame (raising ()
                                       (if label
                                           (labelling (:label label
                                                       :name 'label
                                                       :label-alignment :top)
                                             container)
                                           container))
                                     :left nil
                                     :top nil)))
        (adopt-frame fm frame)
        (setf (slot-value menu-stream 'menu-frame) frame)
        (unwind-protect
             (progn
               (setf (stream-end-of-line-action menu-stream) :allow
                     (stream-end-of-page-action menu-stream) :allow)
               (funcall continuation menu-stream))
          (when deexpose ; Checkme as well.
            (disown-frame fm frame)))))))

(defmethod menu-choose
    (items &rest args &key associated-window &allow-other-keys)
  (let* ((associated-frame (if associated-window
                               (pane-frame associated-window)
                               *application-frame*))
         (frame-manager (frame-manager associated-frame)))
    (apply #'frame-manager-menu-choose frame-manager items args)))

(defmethod frame-manager-menu-choose
    (frame-manager items    ; XXX specialize on STANDARD-FRAME-MANAGER
     &rest options
     &key associated-window printer presentation-type
     (default-item nil default-item-p)
     text-style label cache unique-id id-test cache-value cache-test
     max-width max-height n-rows (n-columns 1) x-spacing y-spacing row-wise
     cell-align-x cell-align-y (scroll-bars :vertical)
     ;; We provide pointer documentation by default.
     (pointer-documentation *pointer-documentation-output*))
  (flet ((drawer (stream type)
           (draw-standard-menu stream type items
                               (if default-item-p
                                   default-item
                                   (first items))
                               :item-printer
                               (cond
                                 (presentation-type
                                  (lambda (menu-item stream)
                                    (present menu-item presentation-type :stream stream)))
                                 (printer printer)
                                 (t #'print-menu-item))
                               :max-width max-width
                               :max-height max-height
                               :n-rows n-rows
                               :n-columns n-columns
                               :x-spacing x-spacing
                               :y-spacing y-spacing
                               :row-wise row-wise
                               :cell-align-x cell-align-x
                               :cell-align-y cell-align-y)))
    (multiple-value-bind (object event)
        (with-menu (menu associated-window
                         :label label
                         :scroll-bars scroll-bars)
          (when text-style
            (setf (medium-text-style menu) text-style))
          (letf (((stream-default-view menu) +textual-menu-view+))
            (menu-choose-from-drawer menu (or presentation-type 'menu-item)
                                     #'drawer
                                     :cache cache
                                     :unique-id unique-id
                                     :id-test id-test
                                     :cache-value cache-value
                                     :cache-test cache-test
                                     :pointer-documentation pointer-documentation)))
      (unless (null event)              ; Event is NIL if user aborted.
        (let ((subitems (menu-item-option object :items 'menu-item-no-items)))
          (if (eq subitems 'menu-item-no-items)
              (values (menu-item-value object) object event)
              (apply #'frame-manager-menu-choose
                     frame-manager subitems
                     options)))))))

(defun max-x-y (frame)
  "Return the maximum X and Y coordinate values for a menu for
`frame' (essentially, the screen resolution with a slight
padding.)"
  ;; FIXME? There may be a better way.
  (let* ((port (port (frame-manager frame)))
         (graft (find-graft :port port)))
    (values (- (graft-width graft) 50)
            (- (graft-height graft) 50))))

(defun menu-size (menu frame)
  "Return two values, the height and width of MENU (adjusted for
maximum size according to `frame')."
  (multiple-value-bind (max-width max-height) (max-x-y frame)
    (with-bounding-rectangle* (:x2 x2 :y2 y2) menu
      (values (min x2 max-width)
              (min y2 max-height)))))

(defmethod adjust-menu-size-and-position ((menu menu-pane)
                                          &key x-position y-position)
  ;; Make sure the menu isn't higher or wider than the screen.
  (multiple-value-bind (menu-width menu-height)
      (menu-size (stream-output-history menu) *application-frame*)
    (change-space-requirements menu
                               :width menu-width
                               :height menu-height
                               :resize-frame t)
    ;; If we have scroll-bars, we need to do some calibration of the size of
    ;; the viewport.
    (when-let ((viewport (pane-viewport menu))
               (scroller (pane-scroller menu)))
      (multiple-value-bind (viewport-width viewport-height)
          (menu-size viewport *application-frame*)
        (multiple-value-bind (scroller-width scroller-height)
            (bounding-rectangle-size scroller)
          (let ((width (+ menu-width (- scroller-width viewport-width)))
                (height (+ menu-height (- scroller-height viewport-height))))
            ;; HACK: How are you supposed to change the size of the viewport?
            ;; I could only find this way, where I calculate the size
            ;; difference between the viewport and the scroller pane, and set
            ;; the scroller pane to the desired size of the viewport, plus the
            ;; difference (to make room for scroll bars).
            (change-space-requirements scroller
                                       :width width
                                       :height height
                                       :resize-frame t)))))
    ;; Modify the size and location of the frame as well.
    (let ((top-level-pane (get-top-level-sheet menu)))
      (multiple-value-bind (frame-width frame-height)
          (menu-size top-level-pane *application-frame*)
        (multiple-value-bind (res-max-x res-max-y) (max-x-y *application-frame*)
          ;; Move the menu frame so that no entries are outside the visible
          ;; part of the screen.
          (let ((max-left (- res-max-x frame-width))
                (max-top (- res-max-y frame-height)))
            (multiple-value-bind (left top)
                (transform-position (sheet-transformation top-level-pane) 0 0)
              (when x-position
                (setf left x-position))
              (when y-position
                (setf top y-position))
              ;; Adjust for maximum position if the programmer has not
              ;; explicitly provided coordinates.
              (when (and (null x-position) (> left max-left))
                (setf left max-left))
              (when (and (null y-position) (> top max-top))
                (setf top max-top))
              (move-sheet top-level-pane
                          (max left 0) (max top 0)))))))))

(defmethod adjust-menu-size-and-position (menu &key &allow-other-keys)
  (declare (ignore menu))
  ;; Nothing.
  nil)

(define-gesture-name menu-choose-exit :keyboard :escape)

(defvar *menu-choose-abort-gestures*
  (list 'menu-choose-exit)
  "A list of gesture names that serve as additional abort gestures for
`menu-choose-from-drawer'.")

;; Spec function.
(defmethod menu-choose-from-drawer
    (menu presentation-type drawer
     &key x-position y-position cache unique-id id-test cache-value cache-test
     default-presentation pointer-documentation)
  (declare (ignore cache unique-id
                   id-test cache-value cache-test default-presentation))
  (with-room-for-graphics (menu :first-quadrant nil)
    (funcall drawer menu presentation-type))

  (adjust-menu-size-and-position menu :x-position x-position
                                      :y-position y-position)
  ;; The menu is enabled (make visible) after the size is adjusted.
  (enable-menu menu)
  (let ((*pointer-documentation-output* pointer-documentation)
        (*abort-gestures* (append *menu-choose-abort-gestures*
                                  *abort-gestures*)))
    (handler-case
        (with-input-context (`(or ,presentation-type blank-area) :override t)
            (object type event)
            (prog1 nil (loop (read-gesture :stream menu)))
          (blank-area nil)
          (t (values object event)))
      (abort-gesture () nil))))
