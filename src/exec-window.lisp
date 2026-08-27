(in-package #:lispbsd)

;;;; -- Exec Window --

(defclass exec-window ()
  ((exec-window-exec
    :initarg :exec
    :reader exec-window-exec
    :documentation "The Exec this window evaluates into.")
   (exec-window-window
    :initarg :window
    :reader exec-window-window
    :documentation "The desktop window presenting the transcript.")
   (exec-window-input
    :initform ""
    :accessor exec-window-input
    :type string
    :documentation "The line currently being typed."))
  (:documentation
   "A Lisp Exec presented as an interactive desktop window."))


(defgeneric exec-window-handle-event (exec-window event)
  (:documentation
   "React to an input EVENT routed to EXEC-WINDOW's window.

Key presses edit the input line; Return evaluates it. Other events are
ignored. Returns EXEC-WINDOW."))


(defmethod exec-window-handle-event ((exec-window exec-window)
                                     (event input-event))
  exec-window)


(defmethod exec-window-handle-event ((exec-window exec-window)
                                     (event pointer-event))
  (when (eq (pointer-event-action event) ':press)
    (let* ((window (exec-window-window exec-window))
           (presentation (window-presentation-at window
                                                 (pointer-event-x event)
                                                 (pointer-event-y event)))
           (desktop (window-desktop window)))
      (when (and presentation desktop)
        (if (eq (pointer-event-button event) ':right)
            (desktop-open-menu desktop
                               (exec-window--presentation-menu-items
                                exec-window presentation desktop)
                               :x (pointer-event-x event)
                               :y (pointer-event-y event))
            (exec-window--inspect-presentation exec-window presentation
                                               desktop)))))
  exec-window)


(defmethod exec-window-handle-event ((exec-window exec-window)
                                     (event key-event))
  (when (eq (key-event-action event) ':press)
    (let ((key (key-event-key event))
          (character (key-event-character event)))
      (cond ((eq key ':return)
             (exec-window--submit exec-window))
            ((eq key ':backspace)
             (exec-window--erase exec-window))
            ((and character (graphic-char-p character))
             (exec-window--insert exec-window character))))
    (exec-window-repaint exec-window))
  exec-window)


(-> make-exec-window (&key (:exec (option exec)) (:world t) (:package package)
                          (:title string) (:x integer) (:y integer)
                          (:width integer) (:height integer))
    exec-window)
(defun make-exec-window (&key exec (world *world*) (package *package*)
                         (title "Exec") (x 0) (y 0) (width 320) (height 200))
  "Return an Exec presented in a fresh detached window.

When EXEC is not supplied, a new Exec bound to WORLD and PACKAGE is
created. The window's event handler feeds input to the Exec."
  (let* ((exec-object (or exec (make-exec :world world :package package)))
         (window (make-window :title title
                              :x x
                              :y y
                              :width width
                              :height height))
         (exec-window (make-instance 'exec-window
                                     :exec exec-object
                                     :window window)))
    (setf (window-application window) exec-window)
    (setf (window-event-handler window)
          (lambda (window event)
            (declare (ignore window))
            (exec-window-handle-event exec-window event)))
    (exec-window-repaint exec-window)
    exec-window))


(-> exec-window-repaint (exec-window) exec-window)
(defun exec-window-repaint (exec-window)
  "Redraw the transcript and input line into the window content bitmap.

The most recent lines that fit are shown, and a block cursor follows
the input line. Visible transcript lines are recorded as presentations
of their forms, values, and conditions."
  (let* ((window (exec-window-window exec-window))
         (content (window-content-bitmap window))
         (font *system-font*)
         (visible-count (max 1 (floor (1- (bitmap-height content))
                                      (window-line-height))))
         (lines (exec-window--lines exec-window))
         (dropped (max 0 (- (length lines) visible-count)))
         (visible (nthcdr dropped lines)))
    (bitmap-clear content)
    (window-clear-presentations window)
    (loop for line in visible
          for index from 0
          for text = (first line)
          for presented = (rest line)
          do (bitmap-draw-text content font text
                               :x *window-text-margin*
                               :y (window-line-y index))
             (when presented
               (window-present window (rest presented)
                               :type (first presented)
                               :x *window-text-margin*
                               :y (window-line-y index)
                               :width (font-text-width font text)
                               :height (font-height font))))
    (let ((cursor-x (+ *window-text-margin*
                       (font-text-width font (first (first (last visible))))))
          (cursor-y (window-line-y (1- (length visible)))))
      (bitmap-fill content :x cursor-x
                           :y cursor-y
                           :width (glyph-advance (font-glyph font #\Space))
                           :height (font-height font))))
  exec-window)


(-> exec-window--lines (exec-window) list)
(defun exec-window--lines (exec-window)
  "Return the Exec transcript as line records, oldest first.

Each record is (text . (type . object)) where the trailing pair names
what the line presents, or (text . NIL) for the input line. The final
record is the input line prefixed with the prompt."
  (let ((exec (exec-window-exec exec-window))
        (lines nil))
    (let ((*package* (exec-package exec))
          (*print-length* *window-print-length*)
          (*print-level* *window-print-level*))
      (dolist (entry (exec-history exec))
        (push (cons (format nil "> ~S" (exec-entry-form entry))
                    (cons ':form (exec-entry-form entry)))
              lines)
        (let ((condition (exec-entry-condition entry)))
          (if condition
              (push (cons (format nil "Error: ~A" condition)
                          (cons ':condition condition))
                    lines)
              (dolist (value (exec-entry-values entry))
                (push (cons (format nil "~S" value)
                            (cons ':value value))
                      lines))))))
    (push (cons (format nil "> ~A" (exec-window-input exec-window)) nil)
          lines)
    (nreverse lines)))


(-> exec-window--insert (exec-window character) exec-window)
(defun exec-window--insert (exec-window character)
  "Append CHARACTER to the input line."
  (setf (exec-window-input exec-window)
        (concatenate 'string
                     (exec-window-input exec-window)
                     (string character)))
  exec-window)


(-> exec-window--erase (exec-window) exec-window)
(defun exec-window--erase (exec-window)
  "Remove the last character of the input line, when there is one."
  (let ((input (exec-window-input exec-window)))
    (when (plusp (length input))
      (setf (exec-window-input exec-window)
            (subseq input 0 (1- (length input))))))
  exec-window)


(-> exec-window--presentation-menu-items (exec-window presentation desktop)
    list)
(defun exec-window--presentation-menu-items (exec-window presentation desktop)
  "Return the context menu entries for PRESENTATION."
  (list (make-menu-item :label "Inspect"
                        :value presentation
                        :action (lambda ()
                                  (exec-window--inspect-presentation
                                   exec-window presentation desktop)))))


(-> exec-window--inspect-presentation (exec-window presentation desktop)
    inspector-window)
(defun exec-window--inspect-presentation (exec-window presentation desktop)
  "Open an inspector on PRESENTATION's object beside the Exec window."
  (let* ((window (exec-window-window exec-window))
         (inspector-window (make-inspector-window
                            :object (presentation-object presentation)
                            :x (+ (window-x window) 24)
                            :y (+ (window-y window) 24))))
    (desktop-attach-window desktop (inspector-window-window inspector-window))
    inspector-window))


(-> exec-window--submit (exec-window) exec-window)
(defun exec-window--submit (exec-window)
  "Evaluate the input line in the Exec and clear it.

Whitespace-only input is cleared without evaluation."
  (let ((input (string-trim " " (exec-window-input exec-window))))
    (when (plusp (length input))
      (exec-evaluate (exec-window-exec exec-window) input))
    (setf (exec-window-input exec-window) ""))
  exec-window)
