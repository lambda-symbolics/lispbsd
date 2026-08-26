(in-package #:lispbsd)

;;;; -- Generic Inspector --

(defgeneric inspect-parts (object)
  (:documentation
   "Return a list of (name . value) parts describing OBJECT."))


(defmethod inspect-parts ((object t))
  (list (cons :type (type-of object))
        (cons :identity (sxhash object))))


(defmethod inspect-parts ((object symbol))
  (list (cons :name (symbol-name object))
        (cons :package (symbol-package object))
        (cons :boundp (boundp object))
        (cons :fboundp (fboundp object))
        (cons :value (and (boundp object) (symbol-value object)))))


(defmethod inspect-parts ((object cons))
  (list (cons :first (first object))
        (cons :rest (rest object))
        (cons :list-p (listp (rest object)))))


(defmethod inspect-parts ((object string))
  (list (cons :length (length object))
        (cons :contents object)))


(defmethod inspect-parts ((object vector))
  (list (cons :length (length object))
        (cons :element-type (array-element-type object))
        (cons :contents (coerce object 'list))))


(defmethod inspect-parts ((object hash-table))
  (let ((entries nil))
    (maphash (lambda (key value)
               (push (cons key value) entries))
             object)
    (list (cons :test (hash-table-test object))
          (cons :count (hash-table-count object))
          (cons :entries (nreverse entries)))))


(defmethod inspect-parts ((object standard-object))
  (let ((parts (list (cons :class (class-name (class-of object))))))
    (dolist (slot (class-slots (class-of object)))
      (let ((name (slot-definition-name slot)))
        (push (cons name
                    (if (slot-boundp object name)
                        (slot-value object name)
                        ':unbound))
              parts)))
    (nreverse parts)))
