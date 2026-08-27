(in-package #:lispbsd)

;;;; -- Presentations --

(defclass presentation ()
  ((presentation-object
    :initarg :object
    :reader presentation-object
    :documentation "The live object this presentation shows.")
   (presentation-type
    :initarg :type
    :reader presentation-type
    :documentation "Semantic presentation type, for example ':value.")
   (presentation-x
    :initarg :x
    :reader presentation-x
    :type integer
    :documentation "Left edge in window content coordinates.")
   (presentation-y
    :initarg :y
    :reader presentation-y
    :type integer
    :documentation "Top edge in window content coordinates.")
   (presentation-width
    :initarg :width
    :reader presentation-width
    :type integer
    :documentation "Width of the presented region in pixels.")
   (presentation-height
    :initarg :height
    :reader presentation-height
    :type integer
    :documentation "Height of the presented region in pixels."))
  (:documentation
   "An association between visible window output and a live object."))


(-> make-presentation (&key (:object t) (:type keyword) (:x integer)
                           (:y integer) (:width integer) (:height integer))
    presentation)
(defun make-presentation (&key object (type ':object) (x 0) (y 0)
                          (width 0) (height 0))
  "Return a presentation of OBJECT covering a window content region."
  (make-instance 'presentation
                 :object object
                 :type type
                 :x x
                 :y y
                 :width width
                 :height height))


(-> presentation-contains-point-p (presentation integer integer) boolean)
(defun presentation-contains-point-p (presentation x y)
  "Return true when content point (X, Y) lies inside PRESENTATION."
  (and (>= x (presentation-x presentation))
       (>= y (presentation-y presentation))
       (< x (+ (presentation-x presentation)
               (presentation-width presentation)))
       (< y (+ (presentation-y presentation)
               (presentation-height presentation)))
       t))


(-> window-present (window t &key (:type keyword) (:x integer) (:y integer)
                          (:width integer) (:height integer))
    presentation)
(defun window-present (window object &key (type ':object) (x 0) (y 0)
                       (width 0) (height 0))
  "Record a presentation of OBJECT over a content region of WINDOW.

Later presentations are found before earlier ones at the same point.
Returns the presentation."
  (let ((presentation (make-presentation :object object
                                         :type type
                                         :x x
                                         :y y
                                         :width width
                                         :height height)))
    (push presentation (window-presentations window))
    presentation))


(-> window-clear-presentations (window) window)
(defun window-clear-presentations (window)
  "Forget all presentations recorded on WINDOW."
  (setf (window-presentations window) nil)
  window)


(-> window-presentation-at (window integer integer) (option presentation))
(defun window-presentation-at (window x y)
  "Return the newest presentation under desktop point (X, Y), or NIL."
  (multiple-value-bind (content-x content-y)
      (window-point->content window x y)
    (when content-x
      (find-if (lambda (presentation)
                 (presentation-contains-point-p presentation
                                                content-x
                                                content-y))
               (window-presentations window)))))
