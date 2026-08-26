(in-package #:lispbsd)

;;;; -- Fundamental Types --

(deftype option (inner-type)
  "A value that is either NIL or an instance of INNER-TYPE."
  `(or null ,inner-type))

(deftype timestamp ()
  "A Common Lisp universal-time timestamp."
  '(integer 0))


(-> non-empty-string-p (t) boolean)
(defun non-empty-string-p (value)
  "Return true when VALUE is a string containing a non-whitespace character."
  (and (stringp value)
       (not (every (lambda (character)
                     (find character
                           '(#\Space #\Tab #\Newline #\Return #\Page)))
                   value))))

(deftype non-empty-string ()
  "A string containing at least one non-whitespace character."
  '(satisfies non-empty-string-p))


(-> object-id-p (t) boolean)
(defun object-id-p (value)
  "Return true when VALUE is a 32-character lowercase hexadecimal identifier."
  (and (stringp value)
       (= (length value) 32)
       (every (lambda (character)
                (digit-char-p character 16))
              value)))

(deftype object-id ()
  "An opaque 32-character hexadecimal object identifier."
  '(satisfies object-id-p))


(deftype activity-state ()
  "Lifecycle state of a schedulable world activity."
  '(member :new :runnable :waiting :suspended :debugging :quiescing :stopped :failed))

(deftype world-phase ()
  "Startup or shutdown phase of a live world."
  '(member :unborn
           :runtime
           :generation-validated
           :stores-attached
           :world-loaded
           :machine-attached
           :activities-started
           :session-established
           :operational
           :shutting-down
           :stopped))

(deftype authority-operation ()
  "A named operation granted by an authority object."
  '(and keyword (satisfies identity)))


(-> make-object-id () object-id)
(defun make-object-id ()
  "Return a new opaque hexadecimal object identifier."
  (format nil "~32,'0x" (random (expt 16 32))))


(-> current-timestamp () timestamp)
(defun current-timestamp ()
  "Return the current universal time."
  (get-universal-time))
