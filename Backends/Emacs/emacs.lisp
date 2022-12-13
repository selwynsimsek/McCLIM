;;; emacs.lisp -- Experimental McCLIM backend for Emacs (via SLIME)
;;;
;;; This file provides a backend to render McCLIM graphics in Emacs via SLIME.
;;; CLIM drawing operations are mapped onto an SVG canvas and shipped to Emacs
;;; via socket connection.

(in-package #:common-lisp-user)
(defpackage #:clim-emacs
  (:use #:clim #:climi #:clime #:climb #:clim-lisp)
  (:import-from #:climi #:left #:right #:top #:bottom
                #:filled #:ink
                #:center-x #:center-y
                #:radius-1-dx #:radius-1-dy
                #:radius-2-dx #:radius-2-dy
                #:draw-rectangle-output-record #:draw-rectangles-output-record
                #:draw-ellipse-output-record
                #:draw-polygon-output-record #:draw-text-output-record
                #:draw-point-output-record #:draw-points-output-record
                #:draw-line-output-record #:draw-lines-output-record))



(in-package #:clim-emacs)
(declaim (optimize (debug 3) (safety 3) (speed 1)))


;;;; Port

(defvar *emacs-command-table* (make-command-table "Emacs"))

(defclass emacs-port (basic-port)
  ((id)))

(defmethod find-port-type ((type (eql :emacs)))
  (values 'emacs-port 'identity))

(defmethod initialize-instance :after ((port emacs-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'id) (gensym "EMACS-PORT-")))


;;;; Medium

(defclass emacs-medium (basic-medium)
  ())

(defmethod make-medium ((port emacs-port) sheet)
  (make-instance 'emacs-medium :port port :sheet sheet))

(defmethod medium-draw-rectangle* ((medium emacs-medium) x1 y1 x2 y2 filled))
(defmethod medium-draw-polygon* ((medium emacs-medium) coord-seq closed filled))
(defmethod medium-draw-ellipse* ((medium emacs-medium) cx cy r1dx r1dy r2dx r2dy sa ea filled))
(defmethod medium-draw-text* ((medium emacs-medium) string x y start end align-x align-y toward-x toward-y transform-glyphs))
(defmethod medium-draw-line* ((medium emacs-medium) x1 y1 x2 y2))
(defmethod medium-draw-lines* ((medium emacs-medium) coord-seq))
(defmethod medium-draw-point* ((medium emacs-medium) x y))
(defmethod medium-draw-points* ((medium emacs-medium) coord-seq))

;;;; Text

;; FIXME We don't provide an implementation of `text-style-mapping`,
;;  and perhaps we don't want to - jqs 2020-05-08

(defmethod climb:text-bounding-rectangle* ((medium emacs-medium) string
                                           &key text-style start end align-x align-y direction)
  (declare (ignore align-x align-y direction))
  (let* ((sub (subseq string (or start 0) (or end (length string))))
         (text-style (or text-style (medium-text-style medium))))
    (multiple-value-bind (width height x y baseline)
        (text-size medium sub :text-style text-style)
      (declare (ignore x y))
      (values 0 (- baseline) width (- height baseline)))))

(defmethod text-size ((medium emacs-medium) string &key text-style (start 0) end)
  (let* ((string (string string))
         (text-style (or text-style (medium-text-style medium)))
         (end (or end (length string)))
         (line-height (text-style-height text-style medium))
         (total-height 0)
         (width 0)
         (max-width 0))
    (climi::dolines (line (subseq string start end)
                          (values max-width total-height
                                  width (- total-height line-height)
                                  (- total-height (text-style-descent text-style medium))))
      (setf width (if (zerop (length line))
                      0
                      (car (get-svg-string-size line text-style))))
      (incf total-height line-height)
      (alexandria:maxf max-width width))))

;; FIXME - hacky, but perhaps we don't want to bother with real text metrics - jqs 2020-05-08

(defparameter *text-style-metrics-cache*
  (make-hash-table :test #'equal)
  "A hash-table, the KEYs of which are lists (family face size), and the VALUES of which are
four-element vector: width, height, ascent, descent")

(defun tsmetric->index (metric)
  (ecase metric
    (:width 0)
    (:height 1)
    (:ascent 2)
    (:descent 3)))

(defun make-text-style-metrics-cache-entry ()
  (make-array 4 :initial-element nil))

(defun text-style-metrics-cache-key (text-style)
  (list (text-style-family text-style)
        (text-style-face text-style)
        (text-style-size text-style)))

(defun get-text-style-metric (text-style metric if-not-found)
  (let ((key (text-style-metrics-cache-key text-style))
        (index (tsmetric->index metric)))
    (multiple-value-bind (entry foundp)
        (gethash key *text-style-metrics-cache*)
      (if foundp
          (alexandria:if-let ((val (svref entry index)))
            val
            (setf (svref entry index) (funcall if-not-found)))
          (let ((entry (make-text-style-metrics-cache-entry)))
            (prog1 (setf (svref entry index) (funcall if-not-found))
              (setf (gethash key *text-style-metrics-cache*) entry)))))))

(defun text-style-base (text-style)
  (svg-image-size (text-to-svg "M" text-style)))

(defmethod text-style-width ((text-style standard-text-style) (medium emacs-medium))
  (get-text-style-metric text-style
                         :width
                         #'(lambda () (car (text-style-base text-style)))))

(defmethod text-style-ascent ((text-style standard-text-style) (medium emacs-medium))
  (get-text-style-metric text-style
                         :ascent
                         #'(lambda () (cdr (svg-image-size (text-to-svg "A" text-style))))))

(defmethod text-style-descent ((text-style standard-text-style) (medium emacs-medium))
  (get-text-style-metric text-style
                         :descent
                         #'(lambda () (- (cdr (svg-image-size (text-to-svg "y" text-style)))
                                         (cdr (svg-image-size (text-to-svg "v" text-style)))))))

(defmethod text-style-height ((text-style standard-text-style) (medium emacs-medium))
  (get-text-style-metric text-style
                         :height
                         #'(lambda () (+ (text-style-ascent text-style medium)
                                         (text-style-descent text-style medium)))))

(defmethod text-style-fixed-width-p ((text-style standard-text-style) (medium emacs-medium))
  (eq :fix (text-style-family text-style)))

;;;; Stream

(defclass clim-emacs-stream (sheet-leaf-mixin
                             sheet-parent-mixin
                             sheet-transformation-mixin
                             sheet-mute-input-mixin
                             sheet-mute-repainting-mixin
                             climi::updating-output-stream-mixin
                             basic-sheet
                             standard-extended-output-stream
                             extended-input-stream
                             permanent-medium-sheet-output-mixin
                             standard-output-recording-stream)
  ((port :initform nil :initarg port :accessor port)))

(defmacro with-output-to-emacs ((stream-var) &body body)
  (let ((cont (gensym)))
    `(flet ((,cont (,stream-var)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-output-to-emacs-stream #',cont))))

(defun invoke-with-output-to-emacs-stream (continuation)
  (with-port (port :emacs)
    (let ((stream (make-instance 'clim-emacs-stream :port port)))
      (sheet-adopt-child (find-graft :port port) stream)
      (prog1 (funcall continuation stream)
        (let ((svg-output (with-output-to-drawing-stream (svg-stream :svg nil)
                            (funcall continuation svg-stream))))
          (swank::send-to-emacs (list :write-clime
                                      (print svg-output)
                                      (presentations-for-emacs stream))))))))

;; FIXME - for some reason CLIM acts as if we have an absurdly small right margin.
;;  For now this can be used in a `with-temporary-margins` call until I work
;;  out how to use it when initializing the stream - jqs 2020-05-08
(defun emacs-right-margin ()
  (swank:ed-rpc 'window-width-for-margin))

(defmethod sheet-region ((sheet clim-emacs-stream)) ; FIXME emacs-graft?
  (make-rectangle* 0 0
                   (emacs-right-margin)
                   100)) ; FIXME ?? perhaps we should do 80 x 43 chars

(defvar *new* nil)

(defun output-record-to-svg (record) ; this is the thing to make use the SVG backend (selwyn)
  (print (if *new*
             (progn
               (with-output-to-drawing-stream (stream :svg nil)
                                        ;(break)
                 (progn
                   (setf (stream-drawing-p stream) t)
                   (replay record stream)
                   (draw-circle* stream 200 200 4 :ink +red+))))
             (multiple-value-bind (x-min y-min x-max y-max) (bounding-rectangle* record)
               (let ((width  (ceiling (- x-max x-min)))
                     (height (ceiling (- y-max y-min))))
                 (shapes-to-svg (output-history-shapes record) width height))))))

(defun presentations-for-emacs (stream)
  (let (ids)
    (multiple-value-bind (x0 y0) (bounding-rectangle* (stream-output-history stream))
      (labels ((visit (record)
                 (when (typep record 'presentation)
                   (push (list (register-presentation record)
                               (emacs-map-area record x0 y0)
                               (tooltip record))
                         ids))
                 (map-over-output-records #'visit record)))
        (visit (stream-output-history stream))))
    ids))

(defun emacs-map-area (record x0 y0)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* record)
    ;; Syntax follows https://www.gnu.org/software/emacs/manual/html_node/elisp/Image-Descriptors.html
    (let ((left   (floor (- x1 x0)))
          (top    (floor (- y1 y0)))
          (right  (ceiling (- x2 x0)))
          (bottom (ceiling (- y2 y0))))
      (cons '#:rect (cons (cons left top) (cons right bottom))))))


;;;; Presentations and input context

(defvar *presentations* (make-array 0 :adjustable t :fill-pointer 0)
  "Vector of presentations (identified by index.)")

(defun register-presentation (presentation)
  (vector-push-extend presentation *presentations*))

#+swank
(defmethod stream-accept ((stream swank/gray::slime-input-stream) type &rest keywords)
  (declare (ignore keywords))
  (presentation-object (elt *presentations*
                            (swank:clime-accept-in-emacs (acceptable-presentations type)))))

(defun acceptable-presentations (presentation-type)
  (loop for index from 0
        for presentation across *presentations*
        when (presentation-typep (presentation-object presentation) presentation-type)
          collect index))


;;;; Tooltips

(defgeneric tooltip (presentation)
  (:documentation "Return a tooltip string describing PRESENTATION.")
  (:method ((p presentation))
    (with-output-to-string (s)
      (let ((*print-right-margin* 60))
        (cl:describe (presentation-object p) s)))))


;;;; Output records

(defvar *debug-output-tree* nil
  "Most recently processed output tree, for debugging purposes.")

;;; Convert McCLIM's internal output record format into a simple list
;;; representation with (0,0) as the upper-left corner.

(defun output-history-shapes (root)
  "Return the list of shapes in the output history rooted at ROOT."
  (setf *debug-output-tree* root)
  (let (shapes)
    (multiple-value-bind (x-min y-min x-max y-max) (bounding-rectangle* root)
      (assert (<= x-min x-max))
      (assert (<= y-min y-max))
      (map-over-output-record-tree (lambda (record)
                                     (push (output-record-to-list record x-min y-min) shapes))
                                   root)
      (reverse (remove nil shapes)))))

(defun map-over-output-record-tree (fn record)
  "Call FN on RECORD and all descendents of RECORD."
  (flet ((visit (child)
           (map-over-output-record-tree fn child)))
  (funcall fn record)
  (map-over-output-records #'visit record)))
