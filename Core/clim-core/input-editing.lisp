;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2001 by Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2006-2008 by Troels Henriksen (athas@sigkill.dk)
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Implementation of the input editing protocol.
;;;

(in-package #:clim-internals)


(with-system-redefinition-allowed
  (when (and (fboundp 'interactive-stream-p)
             (not (typep (fdefinition 'interactive-stream-p)
                         'generic-function)))
    (fmakunbound 'interactive-stream-p))
  (defgeneric interactive-stream-p (stream)
    (:method (stream)
      (cl:interactive-stream-p stream))))

(defclass standard-input-editing-mixin ()
  ((%typeout-record :accessor typeout-record
                    :initform nil
                    :documentation "The output record (if any)
that is the typeout information for this
input-editing-stream. `With-input-editor-typeout' manages this
output record."))
  (:documentation "A mixin implementing some useful standard
behavior for input-editing streams."))

(defmethod typeout-record :around ((stream standard-input-editing-mixin))
  ;; Can't do this in an initform, since we need to proper position...
  (or (call-next-method)
      (let ((record
              (make-instance 'standard-sequence-output-record
                             :x-position 0
                             :y-position (bounding-rectangle-min-y
                                          (input-editing-stream-output-record stream)))))
        (stream-add-output-record (encapsulating-stream-stream stream)
                                  record)
        (setf (typeout-record stream) record))))

(defmacro with-input-editor-typeout ((&optional (stream t) &rest args
                                      &key erase)
                                     &body body)
  "Clear space above the input-editing stream `stream' and
evaluate `body', capturing output done to `stream'. Place will be
obtained above the input-editing area and the output put
there. Nothing will be displayed until `body' finishes. `Stream'
is not evaluated and must be a symbol. If T (the default),
`*standard-input*' will be used. `Stream' will be bound to an
`extended-output-stream' while `body' is being evaluated."
  (declare (ignore erase))
  (check-type stream symbol)
  (let ((stream (if (eq stream t) '*standard-output* stream)))
    `(invoke-with-input-editor-typeout
      ,stream
      #'(lambda (,stream)
          ,@body)
      ,@args)))

(defgeneric invoke-with-input-editor-typeout (stream continuation &key erase)
  (:documentation "Call `continuation' with a single argument, a
stream to do input-editor-typeout on."))

(defun sheet-move-output-vertically (sheet y delta-y)
  "Move the output records of `sheet', starting at vertical
device unit offset `y' or below, down by `delta-y' device units,
then repaint `sheet'."
  (unless (zerop delta-y)
    (with-bounding-rectangle* (:x2 sheet-x2 :y2 sheet-y2) sheet
      (map-over-output-records-overlapping-region
       #'(lambda (record)
           (multiple-value-bind (record-x record-y) (output-record-position record)
             (when (> (+ record-y (bounding-rectangle-height record)) y)
               (setf (output-record-position record)
                     (values record-x (+ record-y delta-y))))))
       (stream-output-history sheet)
       (make-bounding-rectangle 0 y sheet-x2 sheet-y2))
      ;; Only repaint within the visible region...
      (with-bounding-rectangle* (viewport-x1 nil viewport-x2 viewport-y2)
          (sheet-visible-region sheet)
        (dispatch-repaint sheet (make-bounding-rectangle viewport-x1 (- y (abs delta-y))
                                                         viewport-x2 viewport-y2))))))

(defmethod invoke-with-input-editor-typeout ((editing-stream standard-input-editing-mixin)
                                             (continuation function) &key erase)
  (with-accessors ((stream-typeout-record typeout-record)) editing-stream
    ;; Can't do this in an initform, as we need to set the proper
    ;; output record position.
    (let* ((encapsulated-stream (encapsulating-stream-stream editing-stream))
           (old-min-y (bounding-rectangle-min-y stream-typeout-record))
           (old-height (bounding-rectangle-height stream-typeout-record))
           (new-typeout-record (with-output-to-output-record (encapsulated-stream
                                                              'standard-sequence-output-record
                                                              record)
                                 (unless erase
                                   ;; Steal the children of the old typeout record.
                                   (map nil #'(lambda (child)
                                                (setf (output-record-parent child) nil
                                                      (output-record-position child) (values 0 0))
                                                (add-output-record child record))
                                        (output-record-children stream-typeout-record))
                                   ;; Make sure new output is done
                                   ;; after the stolen children.
                                   (stream-increment-cursor-position
                                    encapsulated-stream 0 old-height))
                                 (funcall continuation encapsulated-stream))))
      (when (alexandria:emptyp (output-record-children new-typeout-record))
        (clear-output-record stream-typeout-record)
        (return-from invoke-with-input-editor-typeout))
      (setf (output-record-position new-typeout-record) (values 0 old-min-y))
      ;; Calculate the height difference between the old typeout and the new.
      (let ((delta-y (- (bounding-rectangle-height new-typeout-record) old-height)))
        (multiple-value-bind (typeout-x typeout-y)
            (output-record-position new-typeout-record)
          (declare (ignore typeout-x))
          ;; Clear the old typeout...
          (clear-output-record stream-typeout-record)
          ;; Move stuff for the new typeout record...
          (sheet-move-output-vertically encapsulated-stream typeout-y delta-y)
          ;; Reuse the old stream-typeout-record...
          (add-output-record new-typeout-record stream-typeout-record)
          ;; Now, let there be light!
          (dispatch-repaint encapsulated-stream stream-typeout-record))))))

(defun clear-typeout (&optional (stream t))
  "Blank out the input-editor typeout displayed on `stream',
defaulting to T for `*standard-output*'."
  (with-input-editor-typeout (stream :erase t)
    (declare (ignore stream))))

(defmacro with-input-editing ((&optional (stream t)
                               &rest args
                               &key input-sensitizer (initial-contents "")
                                 (class ''standard-input-editing-stream))
                              &body body)
  "Establishes a context in which the user can edit the input
typed in on the interactive stream `stream'. `Body' is then
executed in this context, and the values returned by `body' are
returned as the values of `with-input-editing'. `Body' may have
zero or more declarations as its first forms.

The stream argument is not evaluated, and must be a symbol that
is bound to an input stream. If stream is T (the default),
`*standard-input*' is used. If stream is a stream that is not an
interactive stream, then `with-input-editing' is equivalent to
progn.

`input-sensitizer', if supplied, is a function of two arguments,
a stream and a continuation function; the function has dynamic
extent. The continuation, supplied by CLIM, is responsible for
displaying output corresponding to the user's input on the
stream. The input-sensitizer function will typically call
`with-output-as-presentation' in order to make the output
produced by the continuation sensitive.

If `initial-contents' is supplied, it must be either a string or
a list of two elements, an object and a presentation type. If it
is a string, the string will be inserted into the input buffer
using `replace-input'. If it is a list, the printed
representation of the object will be inserted into the input
buffer using `presentation-replace-input'."
  (with-stream-designator (stream '*standard-input*)
    (with-keywords-removed (args (:input-sensitizer :initial-contents :class))
      `(invoke-with-input-editing ,stream
                                  #'(lambda (,stream) ,@body)
                                  ,input-sensitizer ,initial-contents
                                  ,class
                                  ,@args))))

(defmacro with-input-position ((stream) &body body)
  (let ((stream-var (gensym "STREAM")))
    `(let* ((,stream-var ,stream)
            (*current-input-stream* (and (typep ,stream-var
                                                'input-editing-stream)
                                         ,stream-var))
            (*current-input-position* (and *current-input-stream*
                                           (stream-scan-pointer ,stream-var))))
       ,@body)))

(define-condition rescan-condition (condition)
  ())

(defun input-editing-rescan-loop (editing-stream continuation)
  (let ((start-scan-pointer (stream-scan-pointer editing-stream)))
    (loop (block rescan
            (handler-bind ((rescan-condition
                             #'(lambda (c)
                                 (declare (ignore c))
                                 (reset-scan-pointer editing-stream start-scan-pointer)
                                 ;; Input-editing contexts above may be interested...
                                 (return-from rescan nil))))
              (return-from input-editing-rescan-loop
                (funcall continuation editing-stream)))))))

(defgeneric finalize (editing-stream input-sensitizer)
  (:documentation "Do any cleanup on an editing stream that is no
longer supposed to be used for editing, like turning off the
cursor, etc."))

(defmethod finalize ((stream t) input-sensitizer)
  (declare (ignore stream input-sensitizer))
  nil)

(defmethod finalize ((stream input-editing-stream) input-sensitizer)
  (declare (ignore input-sensitizer))
  (clear-typeout stream)
  (redraw-input-buffer stream))

(defgeneric invoke-with-input-editing
    (stream continuation input-sensitizer initial-contents class)
  (:documentation "Implements `with-input-editing'. `Class' is
the class of the input-editing stream to create, if necessary."))

(defmethod invoke-with-input-editing
    (stream continuation input-sensitizer initial-contents class)
  (declare (ignore input-sensitizer initial-contents class))
  (funcall continuation stream))

(defmethod invoke-with-input-editing ((stream input-editing-stream)
                                      continuation input-sensitizer
                                      initial-contents class)
  (declare (ignore continuation input-sensitizer class))
  (unless (stream-rescanning-p stream)
    (if (stringp initial-contents)
        (replace-input stream initial-contents)
        (presentation-replace-input stream
                                    (first initial-contents)
                                    (second initial-contents)
                                    (stream-default-view stream))))
  (call-next-method))

(defmethod invoke-with-input-editing :around ((stream extended-output-stream)
                                              continuation
                                              input-sensitizer
                                              initial-contents
                                              class)
  (declare (ignore continuation input-sensitizer initial-contents class))
  (letf (((cursor-visibility (stream-text-cursor stream)) nil))
    (call-next-method)))

(defmethod invoke-with-input-editing :around
    (stream continuation input-sensitizer initial-contents class)
  (declare (ignore stream continuation input-sensitizer initial-contents class))
  (with-activation-gestures (*standard-activation-gestures*)
    (call-next-method)))

(defgeneric input-editing-stream-output-record (stream)
  (:documentation "Return the output record showing the display of the
input-editing stream `stream' values. This function does not
appear in the spec but is used by the command processing code for
layout and to implement a general with-input-editor-typeout."))

(defmethod input-editor-format ((stream t) format-string &rest format-args)
  (unless (and (typep stream 'string-stream)
               (input-stream-p stream))
    (apply #'format stream format-string format-args)))

(defun make-room (buffer pos n)
  (let ((fill (fill-pointer buffer)))
    (when (> (+ fill n)
             (array-dimension buffer 0))
      (adjust-array buffer (list (+ fill n))))
    (incf (fill-pointer buffer) n)
    (replace buffer buffer :start1 (+ pos n) :start2 pos :end2 fill)))

;;; Defaults for replace-input and presentation-replace-input.

(defvar *current-input-stream* nil)
(defvar *current-input-position* 0)

(defun read-token (stream &key
                            (input-wait-handler *input-wait-handler*)
                            (pointer-button-press-handler
                             *pointer-button-press-handler*)
                            click-only)
  "Reads characters from the interactive stream STREAM until it
encounters a delimiter or activation gesture, or a pointer
gesture. Returns the accumulated string that was delimited by the
delimiter or activation gesture, leaving the delimiter unread.

If the first character of typed input is a quotation mark (#\"), then
READ-TOKEN will ignore delimiter gestures until another quotation mark
is seen. When the closing quotation mark is seen, READ-TOKEN will
proceed as above.

INPUT-WAIT-HANDLER and POINTER-BUTTON-PRESS-HANDLER are as for
STREAM-READ-GESTURE."
  (let ((result (make-array 1
                            :adjustable t
                            :fill-pointer 0
                            :element-type 'character))
        (in-quotes nil))
    ;; The spec says that read-token ignores delimiter gestures if the
    ;; first character is #\", until it sees another.  OK... what about
    ;; other occurences of #\"?  Guess we'll just accumulate them.
    (loop for first-char = t then nil
          for gesture = (read-gesture
                         :stream stream
                         :input-wait-handler input-wait-handler
                         :pointer-button-press-handler
                         pointer-button-press-handler)
          do (cond ((or (null gesture)
                        (activation-gesture-p gesture)
                        (typep gesture 'pointer-button-event)
                        (and (not in-quotes)
                             (delimiter-gesture-p gesture)))
                    (loop-finish))
                   ((and (not click-only) (characterp gesture))
                    (if (eql gesture #\")
                        (cond (first-char
                               (setq in-quotes t))
                              (in-quotes
                               (setq in-quotes nil))
                              (t (vector-push-extend gesture result)))
                        (vector-push-extend gesture result)))
                   (t nil))
          finally (progn
                    (when gesture
                      (unread-gesture gesture :stream stream))
                    ;; Return a simple string.  XXX Would returning an
                    ;; adjustable string be so bad?
                    (return (subseq result 0))))))

(defun write-token (token stream &key acceptably)
  "This function is the opposite of `read-token' given the string
token, it writes it to the interactive stream stream. If `acceptably'
is true and there are any characters in the token that are delimiter
gestures (see the macro `with-delimiter-gestures'), then `write-token'
will surround the token with quotation marks (#\").

Typically, `present' methods will use `write-token' instead of
`write-string'."
  (let ((put-in-quotes (and acceptably (some #'delimiter-gesture-p token))))
    (when put-in-quotes
      (write-char #\" stream))
    (write-string token stream)
    (when put-in-quotes
      (write-char #\" stream))))

;;; Signalling Errors Inside present (sic)

(define-condition simple-parse-error (simple-condition parse-error)
  ()
  (:documentation "The error that is signalled by
`simple-parse-error'. This is a subclass of `parse-error'.

This condition handles two initargs, `:format-string' and
`:format-arguments', which are used to specify a control string
and arguments for a call to `format'."))

(defun simple-parse-error (format-string &rest format-args)
  "Signals a `simple-parse-error' error while parsing an input
token. Does not return. `Format-string' and `format-args' are as
for format."
  (error 'simple-parse-error
         :format-control format-string :format-arguments format-args))

(define-condition input-not-of-required-type (parse-error)
  ((string :reader not-required-type-string :initarg :string)
   (type :reader not-required-type-type :initarg :type))
  (:report (lambda (condition stream)
             (format stream "Input ~S is not of required type ~S."
                     (not-required-type-string condition)
                     (not-required-type-type condition))))
  (:documentation "The error that is signalled by
`input-not-of-required-type'. This is a subclass of
`parse-error'.

This condition handles two initargs, `:string' and `:type', which
specify a string to be used in an error message and the expected
presentation type."))

(defun input-not-of-required-type (object type)
  "Reports that input does not satisfy the specified type by
signalling an `input-not-of-required-type' error. `Object' is a
parsed object or an unparsed token (a string). `Type' is a
presentation type specifier. Does not return."
  (error 'input-not-of-required-type :string object :type type))

;;; 24.5 Completion

(defvar *completion-gestures* '(:complete)
  "A list of the gesture names that cause `complete-input' to
complete the user's input as fully as possible. The exact global
contents of this list is unspecified, but must include the
`:complete' gesture name.")

(defvar *help-gestures* '(:help)
  "A list of the gesture names that cause `accept' and
`complete-input' to display a (possibly input context-sensitive)
help message, and for some presentation types a list of
possibilities as well. The exact global contents of this list is
unspecified, but must include the `:help' gesture name.")

(defvar *possibilities-gestures* '(:possibilities)
  "A list of the gesture names that cause `complete-input' to
display a (possibly input context-sensitive) help message and a
list of possibilities. The exact global contents of this list is
unspecified, but must include the `:possibilities' gesture
name.")

(define-condition simple-completion-error (simple-parse-error)
  ((input-so-far :reader completion-error-input-so-far
                 :initarg :input-so-far))
  (:documentation "The error that is signalled by
`complete-input' when no completion is found. This is a subclass
of `simple-parse-error'."))
;;; Helpers for complete-input, which is just getting too long.

(defun complete-gesture-p (gesture)
  (or (delimiter-gesture-p gesture) (activation-gesture-p gesture)))

;;; Break out rescanning case for complete-input.
;;;
;;; funky logic; we don't know if we're still rescanning until after the call
;;; to read-gesture.
(defun complete-input-rescan (stream func partial-completers so-far
                              allow-any-input)
  (when (stream-rescanning-p stream)
    (loop for gesture = (read-gesture :stream stream :timeout 0)
          while (and gesture (stream-rescanning-p stream))
          if (complete-gesture-p gesture)
            do (let (input success object nmatches)
                 (when (gesture-match gesture partial-completers)
                   (setf (values input success object nmatches)
                         (funcall func (subseq so-far 0) :complete-limited)))
                 (unless (and (numberp nmatches) (> nmatches 0))
                   ;; Not a partial match; better be a total match
                   (setf (values input success object)
                         (funcall func (subseq so-far 0) :complete))
                   (if (or success allow-any-input)
                       (progn
                         (unread-gesture gesture :stream stream)
                         (return-from complete-input-rescan
                           (values object t input)))
                       ;; This used to be an error, but no one thought
                       ;; that was a really great idea.
                       (signal 'simple-completion-error
                               :format-control "complete-input: While rescanning,~
                                               can't match ~A~A"
                               :format-arguments (list so-far  gesture)

                               :input-so-far so-far))))
          end
          do (vector-push-extend gesture so-far)
          finally (when gesture
                    (unread-gesture gesture :stream stream))))
  nil)

(defun possibilities-for-menu (possibilities)
  (loop for (display object) in possibilities
        collect `(,display :value ,object)))

(defun possibility-printer (possibility ptype stream)
  "A default function for printing a possibility. Suitable for
used as value of `:possibility-printer' in calls to
`complete-input'"
  (with-output-as-presentation (stream possibility ptype)
    (write-string (first possibility) stream)))

(define-presentation-type possibility ()
  :inherit-from t)

(define-presentation-method presentation-typep (object (type possibility))
  (declare (ignore object))
  t)

(defun print-possibilities (possibilities possibility-printer stream)
  "Write `possibitilies' to `stream', using
`possibility-printer'. `Possibilities' must be a list of
input-completion possibilities. `Stream' must be an input-editing
stream. Output will be done to its typeout."
  (with-input-editor-typeout (stream :erase t)
    (surrounding-output-with-border (stream :shape :drop-shadow :background +cornsilk1+)
      (surrounding-output-with-border (stream :shape :rectangle)
        (format-items possibilities
                      :stream stream
                      :printer #'(lambda (possibility stream)
                                   (funcall possibility-printer
                                            possibility
                                            `((possibility) :description ,(first possibility))
                                            stream)))))))

;;; Helper returns gesture (or nil if gesture shouldn't be part of the input)
;;; and completion mode, if any.

(defvar *completion-possibilities-continuation* nil)

(defun read-completion-gesture (stream
                                partial-completers
                                help-displays-possibilities)
  (flet ((possibilitiesp (gesture)
           (or (gesture-match gesture *possibilities-gestures*)
               (and help-displays-possibilities
                    (gesture-match gesture *help-gestures*)))))
    (let ((*completion-possibilities-continuation*
            #'(lambda ()
                (return-from read-completion-gesture
                  (values nil :possibilities)))))
      (handler-bind ((accelerator-gesture
                       #'(lambda (c)
                           (let ((gesture (accelerator-gesture-event c)))
                             (when (possibilitiesp gesture)
                               (return-from read-completion-gesture
                                 (values nil :possibilities)))))))
        (let ((gesture (read-gesture :stream stream)))
          (values gesture
                  (cond ((possibilitiesp gesture)
                         :possibilities)
                        ((gesture-match gesture partial-completers)
                         :complete-limited)
                        ((gesture-match gesture *completion-gestures*)
                         :complete-maximal)
                        ((complete-gesture-p gesture)
                         :complete)
                        (t nil))))))))

(defparameter *trace-complete-input* nil)

(defun complete-input (stream func &key
                                     partial-completers allow-any-input
                                     (possibility-printer #'possibility-printer)
                                     (help-displays-possibilities t))
  (let ((so-far (make-array 1 :element-type 'character :adjustable t :fill-pointer 0))
        (*accelerator-gestures* (append *help-gestures*
                                        *possibilities-gestures*
                                        *accelerator-gestures*)))
    (with-input-position (stream)
      (flet ((insert-input (input)
               (adjust-array so-far (length input)
                             :fill-pointer (length input))
               (replace so-far input)
               ;; XXX: Relies on non-specified behavior of :rescan.
               (replace-input stream input :rescan nil))
             (read-possibility (stream possibilities)
               (unwind-protect
                    (handler-case
                        (with-input-context
                            (`(completion ,possibilities) :override nil)
                            (object type event)
                            (prog1 nil (read-gesture :stream stream :peek-p t))
                          (t object))
                      (abort-gesture () nil))
                 (clear-typeout stream))))
        (multiple-value-bind (object success input)
            (complete-input-rescan stream func partial-completers
                                   so-far allow-any-input)
          (when success
            (return-from complete-input (values object success input))))
        (loop
          (multiple-value-bind (gesture mode)
              (read-completion-gesture stream
                                       partial-completers
                                       help-displays-possibilities)
            (cond
              (mode
               (multiple-value-bind
                     (input success object nmatches possibilities)
                   (funcall func (subseq so-far 0) mode)
                 (when (and (zerop nmatches)
                            (eq mode :complete-limited)
                            (complete-gesture-p gesture))
                   ;; Gesture is both a partial completer and a
                   ;; delimiter e.g., #\space.  If no partial match,
                   ;; try again with a total match.
                   (setf (values input success object nmatches possibilities)
                         (funcall func (subseq so-far 0) :complete))
                   (setf mode :complete))
                 ;; Preserve the delimiter
                 (when (and success (eq mode :complete))
                   (unread-gesture gesture :stream stream))
                 ;; Get completion from menu
                 (when *trace-complete-input*
                   (format *trace-output* "nmatches = ~A, mode = ~A~%"
                           nmatches mode))
                 (when (and (> nmatches 0) (eq mode :possibilities))
                   (print-possibilities possibilities possibility-printer stream)
                   (redraw-input-buffer stream)
                   (if-let ((possibility (read-possibility stream possibilities)))
                     (setf input (first possibility)
                           object (second possibility)
                           success t
                           nmatches 1)
                     (setf success nil
                           nmatches 0)))
                 (unless (and (eq mode :complete) (not success))
                   (if (> nmatches 0)
                       (insert-input input)
                       (beep)))
                 (cond ((and success (eq mode :complete))
                        (return-from complete-input
                          (values object success input)))
                       ((activation-gesture-p gesture)
                        (if allow-any-input
                            (return-from complete-input
                              (values nil t (subseq so-far 0)))
                            (error 'simple-completion-error
                                   :format-control "Input ~S does not match"
                                   :format-arguments (list so-far)
                                   :input-so-far so-far))))))
              ((null gesture) ; e.g. end-of-input if STREAM is a string stream
               (return-from complete-input (values nil nil so-far)))
              (t
               (vector-push-extend gesture so-far)))))))))

;;; helper function

(defun left-prefix (string1 string2 &key (end nil))
  "Returns the common prefix of string1 and string2, up to end"
  (let* ((end1 (if end
                   (min (length string1) end)
                   nil))
         (end2 (if end
                   (min (length string2) end)
                   nil))
         (mismatch (mismatch string1 string2 :test #'char-equal
                                             :end1 end1 :end2 end2)))
    (cond (mismatch
           (subseq string1 0 mismatch))
          (end
           (subseq string1 0 end))
          (t string1))))

(defun complete-from-generator (initial-string generator delimiters &key
                                                                      (action :complete)
                                                                      (predicate (constantly t)))
  (when (eq action :possibilities)
    (return-from complete-from-generator
      (complete-from-generator-possibilities initial-string
                                             generator
                                             predicate)))
  (let ((initial-string-len (length initial-string))
        (candidate-match nil)
        (matches 0)
        (object nil)
        (identical nil)
        (identical-match nil)
        (identical-object nil)
        (actual-match nil))
    (flet ((suggester (str obj)
             (unless (funcall predicate obj)
               (return-from suggester nil))
             (let ((partial-match-end
                     (and (eq action :complete-limited)
                          (>= (length str) initial-string-len)
                          (position-if #'(lambda (c) (member c delimiters))
                                       str
                                       :start initial-string-len))))
               (when (and (eq action :complete-limited)
                          (null partial-match-end))
                 (return-from suggester nil))
               (unless partial-match-end
                 (setq partial-match-end (1- (length str))))
               (let ((mismatch-initial (mismatch initial-string str
                                                 :test #'char-equal)))
                 (cond ((and mismatch-initial
                             (>= mismatch-initial (length initial-string)))
                        (incf matches)
                        (unless candidate-match
                          (setq object obj))
                        (setf candidate-match
                              (cond (candidate-match
                                     (left-prefix candidate-match
                                                  str
                                                  :end (1+ partial-match-end)))
                                    (partial-match-end
                                     (subseq str 0 (1+ partial-match-end)))
                                    (t str))
                              actual-match str))
                       ((null mismatch-initial)
                        (incf matches)
                        ;; If there's a longer match we want to find it.
                        (if (eq action :complete-maximal)
                            (progn
                              (setf identical-match str)
                              (setf identical-object obj))
                            (progn
                              (setf candidate-match str)
                              (setf object obj)))
                        (setf identical t)))))))
      (funcall generator initial-string #'suggester)
      (let ((partial-match-before-end (and (eq action :complete-limited)
                                           (eql matches 1)
                                           (< (length candidate-match)
                                              (length actual-match)))))
        (values (or candidate-match identical-match initial-string)
                (or (and identical
                         (or (not (eq action :complete-maximal))
                             (eql matches 1)))
                    (and (eql matches 1)
                         (not partial-match-before-end)))
                (if (eq action :complete-maximal)
                    (cond ((and (eql matches 2) identical-match)
                           object)
                          ((and identical-match (eql matches 1))
                           identical-object)
                          ((eql matches 1)
                           object))
                    (and (or identical (and (eql matches 1)
                                            (not partial-match-before-end)))
                         object))
                matches
                nil)))))

;;; The possibilities action is different enough that I don't want to add to
;;; the spaghetti above...

(defun complete-from-generator-possibilities
    (initial-string generator predicate)
  (let ((possibilities nil)
        (nmatches 0)
        (initial-len (length initial-string)))
    (flet ((suggester (str obj)
             (unless (funcall predicate obj)
               (return-from suggester nil))
             (when (>= (or (mismatch initial-string str :test #'char-equal)
                           (length initial-string))
                       initial-len)
               (incf nmatches)
               (push (list str obj) possibilities))))
      (funcall generator initial-string #'suggester)
      (if (and (eql nmatches 1)
               (string-equal initial-string (caar possibilities)))
          ;; return values are as from complete-from-generator, qv.
          (values (caar possibilities)
                  t
                  (cdar possibilities)
                  nmatches
                  possibilities)
          (values initial-string nil nil nmatches (sort possibilities #'string-lessp :key #'car))))))

(defun complete-from-possibilities (initial-string completions delimiters
                                    &key (action :complete)
                                         (predicate (constantly t))
                                         (name-key #'car)
                                         (value-key #'second))
  (flet ((generator (input-string suggester)
           (declare (ignore input-string))
           (do-sequence (possibility completions)
             (funcall suggester
                      (funcall name-key possibility)
                      (funcall value-key possibility)))))
    (complete-from-generator initial-string #'generator delimiters
                             :action action :predicate predicate)))

(defun suggest (completion object)
  "Specifies one possibility for
`completing-from-suggestions'. `Completion' is a string, the
printed representation of object. `Object' is the internal
representation.

Calling this function outside of the body of
`completing-from-suggestions' is an error."
  (declare (ignore completion object))
  (error
   "SUGGEST called outside of lexical scope of COMPLETING-FROM-SUGGESTIONS" ))

(defmacro completing-from-suggestions ((stream &rest args) &body body)
  "Reads input from the input editing stream `stream', completing
over a set of possibilities generated by calls to `suggest'
within `body'. `Body' may have zero or more declarations as its
first forms.

`Completing-from-suggestions' returns three values, `object',
`success', and `string'.

The stream argument is not evaluated, and must be a symbol that
is bound to a stream. If `stream' t is (the default),
`*standard-input*' is used. `Partial-completers',
`allow-any-input', and `possibility-printer' are as for
`complete-input'.

Implementations will probably use `complete-from-generator' to
implement this."
  (when (eq stream t)
    (setq stream '*standard-input*))
  (let ((generator (gensym "GENERATOR"))
        (input-string (gensym "INPUT-STRING"))
        (suggester (gensym "SUGGESTER")))
    `(flet ((,generator (,input-string ,suggester)
              (declare (ignore ,input-string))
              (flet ((suggest (completion object)
                       (funcall ,suggester completion object)))
                ,@body)))
       ;; This sucks, but we can't use args to the macro directly because
       ;; we want the partial-delimiters argument and we need to insure its
       ;; proper evaluation order with everything else.
       (let* ((complete-input-args (list ,@args))
              (partial-completers (getf complete-input-args
                                        :partial-completers
                                        nil)))
         (apply #'complete-input
                ,stream
                #'(lambda (so-far mode)
                    (complete-from-generator so-far
                                             #',generator
                                             partial-completers
                                             :action mode))
                complete-input-args)))))

;;; Infrasructure for detecting empty input, thus allowing accept-1
;;; to supply a default.

(defmacro handle-empty-input ((stream) input-form &body handler-forms)
  "Establishes a context on `stream' (a `standard-input-editing-stream') in
  which empty input entered in `input-form' may transfer control to
  `handler-forms'. Empty input is assumed when a simple-parse-error is
  signalled and there is a delimeter gesture or activation gesture in the
  stream at the position where `input-form' began its input. The gesture that
  caused the transfer remains to be read in `stream'. Control is transferred to
  the outermost `handle-empty-input' form that is empty.

  Note: noise strings in the buffer, such as the prompts of recursive calls to
  `accept', cause input to not be empty. However, the prompt generated by
  `accept' is generally not part of its own empty input context."
  (with-gensyms (input-cont handler-cont)
    `(flet ((,input-cont ()
              ,input-form)
            (,handler-cont ()
              ,@handler-forms))
       (declare (dynamic-extent #',input-cont #',handler-cont))
       (invoke-handle-empty-input ,stream #',input-cont #',handler-cont))))

(define-condition empty-input-condition (simple-condition)
  ((stream :reader empty-input-condition-stream :initarg :stream))
  (:default-initargs :format-control "The input is empty."
                     :format-arguments '()))

;;; The code that signalled the error might have consumed the gesture, or
;;; not.
;;; XXX Actually, it would be a violation of the `accept' protocol to consume
;;; the gesture, but who knows what random accept methods are doing.
(defun empty-input-p
    (stream begin-scan-pointer activation-gestures delimiter-gestures)
  (let ((scan-pointer (stream-scan-pointer stream))
        (fill-pointer (fill-pointer (stream-input-buffer stream))))
    ;; activated?
    (cond ((and (eql begin-scan-pointer scan-pointer)
                (eql scan-pointer fill-pointer))
           t)
          ((or (eql begin-scan-pointer scan-pointer)
               (eql begin-scan-pointer (1- scan-pointer)))
           (let ((gesture (aref (stream-input-buffer stream)
                                begin-scan-pointer)))
             (and (characterp gesture)
                  (or (gesture-match gesture activation-gestures)
                      (gesture-match gesture delimiter-gestures)))))
          (t nil))))

;;; The control flow in here might be a bit confusing. The handler catches
;;; parse errors from accept forms and checks if the input stream is empty. If
;;; so, it resignals an empty-input-condition to see if an outer call to
;;; accept is empty and wishes to handle this situation. We don't resignal the
;;; parse error itself because it might get handled by a handler on ERROR in an
;;; accept method or in user code, which would screw up the default mechanism.
;;;
;;; If the situation is not handled in the innermost empty input handler,
;;; either directly or as a result of resignalling, then it won't be handled
;;; by any of the outer handlers as the stack unwinds, because EMPTY-INPUT-P
;;; will return nil.
(defun invoke-handle-empty-input
    (stream input-continuation handler-continuation)
  (unless (input-editing-stream-p stream)
    (return-from invoke-handle-empty-input (funcall input-continuation)))
  (let ((begin-scan-pointer (stream-scan-pointer stream))
        (activation-gestures *activation-gestures*)
        (delimiter-gestures *delimiter-gestures*))
    (block empty-input
      (handler-bind (((or simple-parse-error empty-input-condition)
                       #'(lambda (c)
                           (when (empty-input-p stream
                                                begin-scan-pointer
                                                activation-gestures
                                                delimiter-gestures)
                             (if (typep c 'empty-input-condition)
                                 (signal c)
                                 (signal 'empty-input-condition :stream stream))
                             ;; No one else wants to handle it, so we will
                             (return-from empty-input nil)))))
        (return-from invoke-handle-empty-input (funcall input-continuation))))
    (funcall handler-continuation)))


;;; I believe this obsolete... --moore
(defmethod presentation-replace-input
    ((stream input-editing-stream) object type view
     &key (buffer-start nil buffer-start-supplied-p)
       (rescan nil rescan-supplied-p)
       query-identifier
       (for-context-type type))
  (declare (ignore query-identifier))
  (let ((result (present-to-string object type
                                   :view view :acceptably nil
                                   :for-context-type for-context-type)))
    (apply #'replace-input stream result
           `(,@(and buffer-start-supplied-p `(:buffer-start ,buffer-start))
             ,@(and rescan-supplied-p `(:rescan ,rescan))))))

;;; For ACCEPT-FROM-STRING, use this barebones input-editing-stream.
(defclass string-input-editing-stream (input-editing-stream fundamental-character-input-stream)
  ((input-buffer :accessor stream-input-buffer)
   (insertion-pointer :accessor stream-insertion-pointer
                      :initform 0
                      :documentation "This is not used for anything at any point.")
   (scan-pointer :accessor stream-scan-pointer
                 :initform 0
                 :documentation "This is not used for anything at any point."))
  (:documentation "An implementation of the input-editing stream
protocol retrieving gestures from a provided string."))

(defmethod initialize-instance :after ((stream string-input-editing-stream)
                                       &key (string (error "A string must be provided"))
                                         (start 0) (end (length string))
                                       &allow-other-keys)
  (setf (stream-input-buffer stream)
        (replace (make-array (- end start) :fill-pointer (- end start))
                 string :start2 start :end2 end)))

(defmethod stream-element-type ((stream string-input-editing-stream))
  'character)

(defmethod close ((stream string-input-editing-stream) &key abort)
  (declare (ignore abort)))

(defmethod stream-peek-char ((stream string-input-editing-stream))
  (or (stream-read-gesture stream :peek-p t)
      :eof))

(defmethod stream-read-char-no-hang ((stream string-input-editing-stream))
  (if (> (stream-scan-pointer stream) (length (stream-input-buffer stream)))
      :eof
      (stream-read-gesture stream)))

(defmethod stream-read-char ((stream string-input-editing-stream))
  (stream-read-gesture stream))

(defmethod stream-listen ((stream string-input-editing-stream))
  (< (stream-scan-pointer stream) (length (stream-input-buffer stream))))

(defmethod stream-unread-char ((stream string-input-editing-stream) char)
  (stream-unread-gesture stream char))

(defmethod invoke-with-input-editor-typeout ((stream string-input-editing-stream) continuation
                                             &key erase)
  (declare (ignore stream continuation erase)))

(defmethod input-editor-format ((stream string-input-editing-stream) format-string
                                &rest args)
  (declare (ignore stream format-string args)))

(defmethod stream-rescanning-p ((stream string-input-editing-stream))
  t)

(defmethod reset-scan-pointer ((stream string-input-editing-stream)
                               &optional scan-pointer)
  (declare (ignore scan-pointer)))

(defmethod immediate-rescan ((stream string-input-editing-stream)))

(defmethod queue-rescan ((stream string-input-editing-stream)))

(defmethod rescan-if-necessary ((stream string-input-editing-stream)
                                &optional inhibit-activation)
  (declare (ignore inhibit-activation)))

(defmethod erase-input-buffer ((stream string-input-editing-stream)
                               &optional start-position)
  (declare (ignore start-position)))

(defmethod redraw-input-buffer ((stream string-input-editing-stream)
                                &optional start-position)
  (declare (ignore start-position)))

(defmethod stream-process-gesture ((stream string-input-editing-stream) gesture type)
  (when (characterp gesture)
    (values gesture type)))

(defmethod stream-read-gesture ((stream string-input-editing-stream)
                                &key peek-p &allow-other-keys)
  (let* ((input-buffer (stream-input-buffer stream))
         (length       (length input-buffer))
         (scan-pointer (stream-scan-pointer stream)))
    (if (> scan-pointer length)
        nil
        (prog1
            (if (= scan-pointer length)
                (second (first (gethash (first *activation-gestures*)
                                        climi::*gesture-names*))) ; XXX - will always be non-NIL?
                (aref input-buffer scan-pointer))
          (unless peek-p
            (incf (stream-scan-pointer stream)))))))

(defmethod stream-unread-gesture ((stream string-input-editing-stream) gesture)
  (declare (ignore gesture))
  (decf (stream-scan-pointer stream)))

(defun accept-1 (stream type
                 &key (view (stream-default-view stream))
                      (default nil defaultp)
                      (default-type nil default-type-p)
                      provide-default
                      insert-default
                      (replace-input t)
                      history
                      active-p
                      prompt
                      prompt-mode
                      display-default
                      query-identifier
                      (activation-gestures nil activationsp)
                      (additional-activation-gestures nil additional-activations-p)
                      (delimiter-gestures nil delimitersp)
                      (additional-delimiter-gestures nil additional-delimiters-p))
  (declare (ignore provide-default history active-p
                   prompt prompt-mode
                   display-default query-identifier))
  (when (and defaultp (not default-type-p))
    (error ":default specified without :default-type"))
  (when (and activationsp additional-activations-p)
    (error "only one of :activation-gestures or ~
            :additional-activation-gestures may be passed to accept."))
  (unless (or activationsp additional-activations-p *activation-gestures*)
    (setq activation-gestures *standard-activation-gestures*))
  (let ((sensitizer-object nil)
        (sensitizer-type 'null))
    (with-input-editing
        (stream
         :input-sensitizer #'(lambda (stream cont)
                               (with-output-as-presentation
                                   (stream sensitizer-object sensitizer-type)
                                 (funcall cont))))
      (with-input-position (stream) ; support for calls to replace-input
        (when (and insert-default
                   (not (stream-rescanning-p stream)))
          ;; Insert the default value to the input stream. It should
          ;; become fully keyboard-editable. We do not want to insert
          ;; the default if we're rescanning, only during initial
          ;; setup.
          (presentation-replace-input stream default default-type view))
        (setf (values sensitizer-object sensitizer-type)
              (with-input-context (type)
                  (object object-type event options)
                  (with-activation-gestures ((if additional-activations-p
                                                 additional-activation-gestures
                                                 activation-gestures)
                                             :override activationsp)
                    (with-delimiter-gestures ((if additional-delimiters-p
                                                  additional-delimiter-gestures
                                                  delimiter-gestures)
                                              :override delimitersp)
                      (let ((accept-results nil))
                        (handle-empty-input (stream)
                            (setq accept-results
                                  (multiple-value-list
                                   (if defaultp
                                       (funcall-presentation-generic-function
                                        accept type stream view
                                        :default default
                                        :default-type default-type)
                                       (funcall-presentation-generic-function
                                        accept type stream view))))
                          ;; User entered activation or delimiter
                          ;; gesture without any input.
                          (if defaultp
                              (progn
                                (presentation-replace-input
                                 stream default default-type view :rescan nil))
                              (simple-parse-error
                               "Empty input for type ~S with no supplied default"
                               type))
                          (setq accept-results (list default default-type)))
                        ;; Eat trailing activation gesture
                        ;; XXX what about pointer gestures?
                        ;; XXX and delimiter gestures?
                        (unless *recursive-accept-p*
                          (let ((ag (read-gesture :stream stream :timeout 0)))
                            (unless (or (null ag) (eq ag stream))
                              (unless (activation-gesture-p ag)
                                (unread-gesture ag :stream stream)))))
                        (values (first accept-results)
                                (if (rest accept-results)
                                    (second accept-results)
                                    type)))))
                ;; A presentation was clicked on, or something
                (t
                 (when (and replace-input
                            (getf options :echo t)
                            (not (stream-rescanning-p stream)))
                   (presentation-replace-input stream object object-type view
                                               :rescan nil))
                 (values object object-type))))
        ;; Just to make it clear that we're returning values
        (values sensitizer-object sensitizer-type)))))

;;; XXX This needs work! It needs to do everything that accept does for
;;; expanding ptypes and setting up recursive call processing
(defun accept-from-string (type string
                           &rest args
                           &key view
                                (default nil defaultp)
                                (default-type nil default-type-p)
                                (activation-gestures nil activationsp)
                                (additional-activation-gestures
                                 nil
                                 additional-activations-p)
                                (delimiter-gestures nil delimitersp)
                                (additional-delimiter-gestures
                                 nil
                                 additional-delimiters-p)
                                (start 0)
                                (end (length string)))
  (declare (ignore view))
  ;; XXX work in progress here.
  (with-activation-gestures ((if additional-activations-p
                                 additional-activation-gestures
                                 activation-gestures)
                             :override activationsp)
    (with-delimiter-gestures ((if additional-delimiters-p
                                  additional-delimiter-gestures
                                  delimiter-gestures)
                              :override delimitersp)))
  (when (zerop (- end start))
    (if defaultp
        (return-from accept-from-string (values default
                                                (if default-type-p
                                                    default-type
                                                    type)
                                                0))
        (simple-parse-error "Empty string")))
  (let ((stream (make-instance 'string-input-editing-stream
                               :string string :start start :end end)))
    (multiple-value-bind (val ptype)
        (with-keywords-removed (args (:start :end))
          (apply #'stream-accept stream type :history nil :view +textual-view+ args))
      (values val ptype (+ (stream-scan-pointer stream) start)))))

(defun accept-using-read
    (stream ptype &key ((:read-eval *read-eval*) nil)
                       (default nil defaultp) (default-type ptype)
     &allow-other-keys)
  (let* ((token (read-token stream)))
    (if (and (string= "" token) defaultp)
        (values default default-type)
        (let ((result (handler-case (read-from-string token)
                        (error (c)
                          (declare (ignore c))
                          (simple-parse-error "Error parsing ~S for presentation type ~S"
                                              token
                                              ptype)))))
          (if (presentation-typep result ptype)
              (values result ptype)
              (input-not-of-required-type result ptype))))))

(defun accept-using-completion (type stream func
                                &rest complete-with-input-key-args)
  "A wrapper around complete-with-input that returns the presentation type with
  the completed object."
  (multiple-value-bind (object success input)
      (apply #'complete-input stream func complete-with-input-key-args)
    (if success
        (values object type)
        (simple-parse-error "Error parsing ~S for presentation type ~S"
                            input
                            type))))

(defmethod stream-accept ((stream string-stream) type
                          &key (view (stream-default-view stream))
                               (default nil defaultp)
                               (default-type nil default-type-p)
                               (activation-gestures nil activationsp)
                               (additional-activation-gestures
                                nil additional-activations-p)
                               (delimiter-gestures nil delimitersp)
                               (additional-delimiter-gestures
                                nil additional-delimiters-p)
                          &allow-other-keys)
  (with-activation-gestures ((if additional-activations-p
                                 additional-activation-gestures
                                 activation-gestures)
                             :override activationsp)
    (with-delimiter-gestures ((if additional-delimiters-p
                                  additional-delimiter-gestures
                                  delimiter-gestures)
                              :override delimitersp)
      (multiple-value-bind (object object-type)
          (apply-presentation-generic-function
           accept
           type stream view
           `(,@(and defaultp `(:default ,default))
             ,@(and default-type-p `(:default-type ,default-type))))
        (values object (or object-type type))))))

(defmethod stream-accept ((stream standard-extended-input-stream) type
                          &rest args
                          &key (view (stream-default-view stream))
                          &allow-other-keys)
  (apply #'prompt-for-accept stream type view args)
  (apply #'accept-1 stream type args))

(defmethod stream-accept ((stream string-input-editing-stream) type &rest args)
  (apply #'accept-1 stream type args))

;;; KLUDGE: ACCEPT-1 is called WITH-EDITING-INPUT while the input
;;; editing depends on presentations. Function is defined in the file
;;; input-editing.lisp.

(defmethod prompt-for-accept ((stream t)
                              type view
                              &rest accept-args
                              &key &allow-other-keys)
  (declare (ignore view))
  (apply #'prompt-for-accept-1 stream type accept-args))

(defun prompt-for-accept-1 (stream type
                            &key
                              (default nil defaultp)
                              (default-type type)
                              (insert-default nil)
                              (prompt t)
                              (prompt-mode :normal)
                              (display-default prompt)
                            &allow-other-keys)
  (flet ((display-using-mode (stream prompt default)
           (ecase prompt-mode
             (:normal
              (if *recursive-accept-p*
                  (input-editor-format stream "(~A~@[[~A]~]) " prompt default)
                  (input-editor-format stream "~A~@[[~A]~]: " prompt default)))
             (:raw
              (input-editor-format stream "~A" prompt)))))
    (let ((prompt-string (if (eq prompt t)
                             (format nil "~:[Enter ~;~]~A"
                                     *recursive-accept-p*
                                     (describe-presentation-type type nil nil))
                             prompt))
          ;; Don't display the default in the prompt if it is to be
          ;; inserted into the input stream.
          (default-string (and defaultp
                               (not insert-default)
                               display-default
                               (present-to-string default default-type))))
      (cond ((null prompt)
             nil)
            (t
             (display-using-mode stream prompt-string default-string))))))

(defmethod prompt-for-accept ((stream string-stream) type view
                              &rest other-args
                              &key &allow-other-keys)
  (declare (ignore type view other-args))
  nil)

;;; When no accept method has been defined for a type, allow some kind of
;;; input.  The accept can be satisfied with pointer input, of course, and this
;;; allows the clever user a way to input the type at the keyboard, using #. or
;;; some other printed representation.
;;;
;;; XXX Once we "go live" we probably want to disable this, probably with a
;;; beep and warning that input must be clicked on.

(define-default-presentation-method accept
    (type stream view &rest args)
  (declare (ignore view))
  (apply #'accept-using-read stream type :read-eval t args))
