(in-package #:lispbsd)

;;;; -- Authority --

(defclass authority ()
  ((authority-id
    :initarg :id
    :initform (make-object-id)
    :reader authority-id
    :type object-id
    :documentation "Stable identifier of this grant.")
   (authority-subject
    :initarg :subject
    :reader authority-subject
    :documentation "Subject permitted to perform the operation.")
   (authority-operation
    :initarg :operation
    :reader authority-operation
    :type keyword
    :documentation "Granted operation, or :all for every operation.")
   (authority-target
    :initarg :target
    :reader authority-target
    :documentation "Target object, or T for every target.")
   (authority-expires-at
    :initarg :expires-at
    :initform nil
    :reader authority-expires-at
    :type (option timestamp)
    :documentation "Universal time after which the grant is inert, or NIL.")
   (authority-delegable-p
    :initarg :delegable-p
    :initform nil
    :reader authority-delegable-p
    :type boolean
    :documentation "True when the subject may delegate this grant.")
   (authority-revoked-p
    :initarg :revoked-p
    :initform nil
    :accessor authority-revoked-p
    :type boolean
    :documentation "True after the grant has been revoked.")
   (authority-provenance
    :initarg :provenance
    :initform nil
    :reader authority-provenance
    :documentation "Grant that this authority was delegated from, if any."))
  (:documentation
   "A first-class grant permitting an operation on a target by a subject."))


(defclass authority-set ()
  ((authority-set-grants
    :initform nil
    :accessor authority-set-grants
    :documentation "Live and revoked grants in this set.")
   (authority-set-lock
    :initform (make-lock "authority-set")
    :reader authority-set-lock
    :documentation "Lock protecting the grant list."))
  (:documentation "A collection of authority grants for one world."))


(-> make-authority-set () authority-set)
(defun make-authority-set ()
  "Return a new empty authority set."
  (make-instance 'authority-set))


(-> authority-live-p (authority) boolean)
(defun authority-live-p (authority)
  "Return true when AUTHORITY is not revoked and has not expired."
  (and (not (authority-revoked-p authority))
       (let ((expires-at (authority-expires-at authority)))
         (or (null expires-at)
             (>= expires-at (current-timestamp))))))


(-> authority-covers-p (authority t keyword t) boolean)
(defun authority-covers-p (authority subject operation target)
  "Return true when AUTHORITY grants OPERATION on TARGET to SUBJECT."
  (and (authority-live-p authority)
       (eq (authority-subject authority) subject)
       (or (eq (authority-operation authority) ':all)
           (eq (authority-operation authority) operation))
       (or (eq (authority-target authority) t)
           (eq (authority-target authority) target))))


(-> grant-authority (authority-set t keyword t
                     &key (:expires-at (option timestamp))
                          (:delegable-p boolean)
                          (:provenance (option authority)))
    authority)
(defun grant-authority (set subject operation target
                        &key expires-at delegable-p provenance)
  "Install a new grant in SET and return it."
  (let ((authority (make-instance 'authority
                                  :subject subject
                                  :operation operation
                                  :target target
                                  :expires-at expires-at
                                  :delegable-p delegable-p
                                  :provenance provenance)))
    (with-lock-held ((authority-set-lock set))
      (push authority (authority-set-grants set)))
    authority))


(-> revoke-authority (authority) authority)
(defun revoke-authority (authority)
  "Revoke AUTHORITY and return it."
  (setf (authority-revoked-p authority) t)
  authority)


(-> authorized-p (authority-set t keyword t) boolean)
(defun authorized-p (set subject operation target)
  "Return true when SET contains a live grant covering the request."
  (with-lock-held ((authority-set-lock set))
    (and (find-if (lambda (authority)
                    (authority-covers-p authority subject operation target))
                  (authority-set-grants set))
         t)))


(-> check-authority (authority-set t keyword t) t)
(defun check-authority (set subject operation target)
  "Signal AUTHORITY-DENIED unless SET grants OPERATION on TARGET to SUBJECT."
  (unless (authorized-p set subject operation target)
    (error 'authority-denied
           :subject subject
           :operation operation
           :target target))
  t)


(-> delegate-authority (authority-set authority t
                        &key (:duration (option integer))
                             (:delegable-p boolean))
    authority)
(defun delegate-authority (set authority new-subject
                           &key duration delegable-p)
  "Delegate AUTHORITY to NEW-SUBJECT inside SET and return the new grant."
  (unless (authority-live-p authority)
    (error 'authority-denied
           :subject (authority-subject authority)
           :operation (authority-operation authority)
           :target (authority-target authority)))
  (unless (authority-delegable-p authority)
    (error 'authority-denied
           :subject (authority-subject authority)
           :operation ':delegate
           :target authority))
  (grant-authority set
                   new-subject
                   (authority-operation authority)
                   (authority-target authority)
                   :expires-at (if duration
                                   (+ (current-timestamp) duration)
                                   (authority-expires-at authority))
                   :delegable-p delegable-p
                   :provenance authority))


(defmacro with-delegated-authority ((set authority new-subject &key duration)
                                    &body body)
  "Delegate AUTHORITY to NEW-SUBJECT for BODY, then revoke the grant."
  (let ((grant (gensym "GRANT")))
    `(let ((,grant (delegate-authority ,set ,authority ,new-subject
                                       :duration ,duration
                                       :delegable-p nil)))
       (unwind-protect
            (progn ,@body)
         (revoke-authority ,grant)))))
