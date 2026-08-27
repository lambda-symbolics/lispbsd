(in-package #:lispbsd)

;;;; -- Inspector Window --

(defclass inspector-window ()
  ((inspector-window-window
    :initarg :window
    :reader inspector-window-window
    :documentation "The desktop window presenting the inspected object.")
   (inspector-window-stack
    :initarg :stack
    :accessor inspector-window-stack
    :documentation "Objects being inspected, current object first.")
   (inspector-window-selection
    :initform 0
    :accessor inspector-window-selection
    :type integer
    :documentation "Index of the selected part of the current object.")
   (inspector-window-package
    :initarg :package
    :reader inspector-window-package
    :documentation "Package used when printing parts."))
  (:documentation
   "A generic object inspector presented as a desktop window."))


(defgeneric inspector-window-handle-event (inspector-window event)
  (:documentation
   "React to an input EVENT routed to INSPECTOR-WINDOW's window.

Up and Down move the selection, Return descends into the selected
part, Backspace ascends, and a pointer press selects the part under
the pointer. Returns INSPECTOR-WINDOW."))


(defmethod inspector-window-handle-event ((inspector-window inspector-window)
                                          (event input-event))
  inspector-window)


(defmethod inspector-window-handle-event ((inspector-window inspector-window)
                                          (event key-event))
  (when (eq (key-event-action event) ':press)
    (case (key-event-key event)
      (:up
       (inspector-window--move-selection inspector-window -1))
      (:down
       (inspector-window--move-selection inspector-window 1))
      (:return
       (inspector-window-descend inspector-window))
      (:backspace
       (inspector-window-ascend inspector-window)))
    (inspector-window-repaint inspector-window))
  inspector-window)


(defmethod inspector-window-handle-event ((inspector-window inspector-window)
                                          (event pointer-event))
  (when (eq (pointer-event-action event) ':press)
    (let ((index (inspector-window--part-index-at inspector-window
                                                  (pointer-event-x event)
                                                  (pointer-event-y event))))
      (when index
        (setf (inspector-window-selection inspector-window) index)
        (inspector-window-repaint inspector-window))))
  inspector-window)


(-> make-inspector-window (&key (:object t) (:title string) (:x integer)
                               (:y integer) (:width integer) (:height integer)
                               (:package package))
    inspector-window)
(defun make-inspector-window (&key object (title "Inspector") (x 0) (y 0)
                              (width 240) (height 160)
                              (package (find-package '#:lispbsd)))
  "Return an inspector on OBJECT presented in a fresh detached window."
  (let* ((window (make-window :title title
                              :x x
                              :y y
                              :width width
                              :height height))
         (inspector-window (make-instance 'inspector-window
                                          :window window
                                          :stack (list object)
                                          :package package)))
    (setf (window-application window) inspector-window)
    (setf (window-event-handler window)
          (lambda (window event)
            (declare (ignore window))
            (inspector-window-handle-event inspector-window event)))
    (inspector-window-repaint inspector-window)
    inspector-window))


(-> inspector-window-object (inspector-window) t)
(defun inspector-window-object (inspector-window)
  "Return the object currently shown by INSPECTOR-WINDOW."
  (first (inspector-window-stack inspector-window)))


(-> inspector-window-descend (inspector-window) inspector-window)
(defun inspector-window-descend (inspector-window)
  "Inspect the value of the selected part of the current object."
  (let ((parts (inspect-parts (inspector-window-object inspector-window)))
        (selection (inspector-window-selection inspector-window)))
    (when (< selection (length parts))
      (push (rest (nth selection parts))
            (inspector-window-stack inspector-window))
      (setf (inspector-window-selection inspector-window) 0)))
  inspector-window)


(-> inspector-window-ascend (inspector-window) inspector-window)
(defun inspector-window-ascend (inspector-window)
  "Return to the previously inspected object, when there is one."
  (when (rest (inspector-window-stack inspector-window))
    (pop (inspector-window-stack inspector-window))
    (setf (inspector-window-selection inspector-window) 0))
  inspector-window)


(-> inspector-window-repaint (inspector-window) inspector-window)
(defun inspector-window-repaint (inspector-window)
  "Redraw the current object and its parts into the window content.

The current object heads the view above a separator line; the selected
part line is drawn inverted. Parts scroll to keep the selection
visible."
  (let* ((window (inspector-window-window inspector-window))
         (content (window-content-bitmap window))
         (font *fixed-font*)
         (object (inspector-window-object inspector-window))
         (parts (inspect-parts object))
         (selection (min (inspector-window-selection inspector-window)
                         (1- (length parts))))
         (first-visible (inspector-window--first-visible inspector-window))
         (visible-count (inspector-window--visible-count inspector-window)))
    (setf (inspector-window-selection inspector-window) selection)
    (bitmap-clear content)
    (let ((*package* (inspector-window-package inspector-window))
          (*print-length* *window-print-length*)
          (*print-level* *window-print-level*))
      (bitmap-draw-text content font (format nil "~S" object)
                        :x *window-text-margin*
                        :y (window-line-y 0))
      (bitmap-draw-line content
                        :x0 0 :y0 (1- (window-line-y 1))
                        :x1 (1- (bitmap-width content))
                        :y1 (1- (window-line-y 1)))
      (loop for index from first-visible
              below (min (length parts) (+ first-visible visible-count))
            for row from 0
            for part = (nth index parts)
            for line-y = (window-line-y (1+ row))
            for text = (format nil "~A: ~S" (first part) (rest part))
            do (if (= index selection)
                   (progn
                     (bitmap-fill content :x 0
                                          :y line-y
                                          :width (bitmap-width content)
                                          :height (font-height font))
                     (bitmap-draw-text content font text
                                       :x *window-text-margin*
                                       :y line-y
                                       :bit 0))
                   (bitmap-draw-text content font text
                                     :x *window-text-margin*
                                     :y line-y)))))
  inspector-window)


(-> inspector-window--move-selection (inspector-window integer) inspector-window)
(defun inspector-window--move-selection (inspector-window delta)
  "Move the selection by DELTA, clamped to the current parts."
  (let ((parts (inspect-parts (inspector-window-object inspector-window))))
    (setf (inspector-window-selection inspector-window)
          (max 0 (min (1- (length parts))
                      (+ (inspector-window-selection inspector-window)
                         delta)))))
  inspector-window)


(-> inspector-window--visible-count (inspector-window) integer)
(defun inspector-window--visible-count (inspector-window)
  "Return how many part lines fit below the header."
  (let ((content (window-content-bitmap
                  (inspector-window-window inspector-window))))
    (max 1 (floor (- (bitmap-height content) (window-line-y 1))
                  (window-line-height)))))


(-> inspector-window--first-visible (inspector-window) integer)
(defun inspector-window--first-visible (inspector-window)
  "Return the index of the first visible part, keeping the selection shown."
  (max 0 (- (1+ (inspector-window-selection inspector-window))
            (inspector-window--visible-count inspector-window))))


(-> inspector-window--part-index-at (inspector-window integer integer)
    (option integer))
(defun inspector-window--part-index-at (inspector-window x y)
  "Return the part index under desktop point (X, Y), or NIL."
  (multiple-value-bind (content-x content-y)
      (window-point->content (inspector-window-window inspector-window) x y)
    (when (and content-x (>= content-y (window-line-y 1)))
      (let ((index (+ (floor (- content-y (window-line-y 1))
                             (window-line-height))
                      (inspector-window--first-visible inspector-window))))
        (when (< index (length (inspect-parts
                                (inspector-window-object inspector-window))))
          index)))))
