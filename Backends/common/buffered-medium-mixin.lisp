;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2022 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; This file provide an implementation of a generic double buffering and it
;;; operates under a few assumptions:
;;;
;;; - drawing functions always operate on the MEDIUM-DRAWABLE
;;; - a pixmap allocated with ALLOCATE-PIXMAP is a valid medium-drawable
;;; - MEDIUM-COPY-AREA works between the "host" drawable and the pixmap
;;;
;;; Output is buffered when MEDIUM-BUFFERING-OUTPUT-T returns T. The function
;;; MEDIUM-DRAWABLE returns then a pixmap suitable for the output.
;;;
;;; Buffer swaps are performed in :before methods of medium-finish-output and
;;; medium-force-output. This implementation is simplistic and it does not
;;; maintain a set of dirty regions.

(in-package #:climi)

(defclass buffered-medium-mixin ()
  ((back-buffer
    :initform nil
    :accessor medium-back-buffer)))

(defmethod invoke-with-output-buffered
    ((medium buffered-medium-mixin) continuation &optional (buffered-p t))
  (when (null buffered-p)
    (medium-force-output medium))
  (unwind-protect (letf (((medium-buffering-output-p medium) buffered-p))
                    (funcall continuation))
    (when (and buffered-p (null (medium-buffering-output-p medium)))
      (medium-force-output medium))))

(defmethod degraft-medium :before ((medium buffered-medium-mixin) port sheet)
  (declare (ignore port sheet))
  (when-let ((back-buffer (medium-back-buffer medium)))
    (deallocate-pixmap back-buffer)
    (setf (medium-back-buffer medium) nil)))

(defun ensure-back-buffer (medium)
  ;; Ensure that a correct back-buffer exists.
  (with-bounding-rectangle* (:x2 x2 :y2 y2)
      (medium-native-region medium)
    (let ((x2 (ceiling x2))
          (y2 (ceiling y2)))
      (if-let ((bb (medium-back-buffer medium)))
        (let ((pw (pixmap-width bb))
              (ph (pixmap-height bb)))
          (when (or (> x2 pw) (> y2 ph))
            (let ((pixmap (allocate-pixmap medium x2 y2)))
              (medium-copy-area bb 0 0 x2 y2 pixmap 0 0)
              (deallocate-pixmap bb)
              (setf (medium-back-buffer medium) pixmap))))
        (letf (((medium-buffering-output-p medium) nil))
          (setf (medium-back-buffer medium)
                (allocate-pixmap medium x2 y2))
          (medium-clear-area medium 0 0 x2 y2)))))
  (medium-back-buffer medium))

(defmethod medium-drawable :around ((medium buffered-medium-mixin))
  (if (medium-buffering-output-p medium)
      (ensure-back-buffer medium)
      (call-next-method)))

(macrolet
    ((frob ()
       `(when (medium-buffering-output-p medium)
          (let ((sheet (medium-sheet medium)))
            (with-bounding-rectangle* (x1 y1 :width width :height height)
                (sheet-native-region sheet)
              (when-let ((back-buffer (medium-back-buffer medium)))
                (medium-copy-area back-buffer x1 y1 width height
                                  (sheet-mirror sheet) x1 y1)))))))
  (defmethod medium-finish-output :after ((medium buffered-medium-mixin))
    (frob))

  (defmethod medium-force-output :after ((medium buffered-medium-mixin))
    (frob)))
