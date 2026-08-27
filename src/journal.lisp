(in-package #:lispbsd)

;;;; -- Mutation Journal --

(define-condition journal-error (lispbsd-error)
  ((journal-error-journal
    :initarg :journal
    :initform nil
    :reader journal-error-journal
    :documentation "The journal involved in the failure, if any."))
  (:documentation "A failure involving a mutation journal."))


(define-condition unwritable-journal-record (journal-error)
  ((unwritable-journal-record-payload
    :initarg :payload
    :reader unwritable-journal-record-payload
    :documentation "The payload that could not be printed readably.")
   (unwritable-journal-record-underlying
    :initarg :underlying
    :reader unwritable-journal-record-underlying
    :documentation "The printer condition that rejected the payload."))
  (:report (lambda (condition stream)
             (format stream "Journal payload does not print readably: ~A."
                     (unwritable-journal-record-underlying condition))))
  (:documentation "A journal payload could not be serialized as readable forms."))


(define-condition invalid-journal-record (journal-error)
  ((invalid-journal-record-form
    :initarg :form
    :reader invalid-journal-record-form
    :documentation "The persisted form that is not a valid record."))
  (:report (lambda (condition stream)
             (format stream "Invalid journal record: ~S."
                     (invalid-journal-record-form condition))))
  (:documentation "A persisted journal form is malformed."))


(defclass journal-record ()
  ((journal-record-id
    :initarg :id
    :reader journal-record-id
    :type object-id
    :documentation "Stable identity of this record.")
   (journal-record-timestamp
    :initarg :timestamp
    :reader journal-record-timestamp
    :type timestamp
    :documentation "Universal time the record was appended.")
   (journal-record-kind
    :initarg :kind
    :reader journal-record-kind
    :documentation "Keyword naming the record kind, for example ':definition-mutation.")
   (journal-record-payload
    :initarg :payload
    :reader journal-record-payload
    :documentation "Readable forms describing the mutation."))
  (:documentation
   "One durable entry in a world's mutation journal."))


(defclass journal ()
  ((journal-path
    :initarg :path
    :reader journal-path
    :documentation "File holding one readable record form per entry.")
   (journal-lock
    :initform (make-lock "journal")
    :reader journal-lock
    :documentation "Serializes appends from concurrent activities."))
  (:documentation
   "An append-only journal of world mutations as readable forms."))


(-> make-journal (&key (:path (or pathname string))) journal)
(defun make-journal (&key path)
  "Return a journal backed by the file at PATH.

The file is created on the first append."
  (make-instance 'journal :path (pathname path)))


(-> journal-append (journal keyword t &key (:id object-id)
                           (:timestamp timestamp))
    journal-record)
(defun journal-append (journal kind payload &key (id (make-object-id))
                       (timestamp (current-timestamp)))
  "Append a KIND record carrying PAYLOAD to JOURNAL and return it.

PAYLOAD must consist of readable forms; an unprintable payload signals
UNWRITABLE-JOURNAL-RECORD before anything is written."
  (let ((text (handler-case
                  (with-standard-io-syntax
                    (let ((*package* (find-package '#:lispbsd)))
                      (prin1-to-string (list ':id id
                                             ':timestamp timestamp
                                             ':kind kind
                                             ':payload payload))))
                (error (condition)
                  (error 'unwritable-journal-record
                         :journal journal
                         :payload payload
                         :underlying condition)))))
    (with-lock-held ((journal-lock journal))
      (with-open-file (stream (journal-path journal)
                              :direction ':output
                              :if-exists ':append
                              :if-does-not-exist ':create)
        (write-line text stream)
        (finish-output stream))))
  (make-instance 'journal-record
                 :id id
                 :timestamp timestamp
                 :kind kind
                 :payload payload))


(-> journal-records (journal) (values list boolean))
(defun journal-records (journal)
  "Return all records of JOURNAL in append order.

Reads with *READ-EVAL* disabled. Returns a second value that is true
when an incomplete final form, as left by a crash mid-append, was
tolerated and ignored."
  (block nil
    (unless (probe-file (journal-path journal))
      (return (values nil nil)))
    (with-open-file (stream (journal-path journal) :direction ':input)
      (let ((records nil))
        (loop
          (let ((form (handler-case
                          (with-standard-io-syntax
                            (let ((*package* (find-package '#:lispbsd))
                                  (*read-eval* nil))
                              (read stream nil ':end-of-journal)))
                        (end-of-file ()
                          (return (values (nreverse records) t)))
                        (reader-error (condition)
                          (error 'invalid-journal-record
                                 :journal journal
                                 :form (princ-to-string condition))))))
            (when (eq form ':end-of-journal)
              (return (values (nreverse records) nil)))
            (push (journal--form->record journal form) records)))))))


(-> journal-mark-durable (journal object-id) journal-record)
(defun journal-mark-durable (journal record-id)
  "Append a durability mark for RECORD-ID and return the mark record."
  (journal-append journal ':durable-mark (list ':marks record-id)))


(-> journal-durable-p (journal object-id) boolean)
(defun journal-durable-p (journal record-id)
  "Return true when RECORD-ID carries a durability mark in JOURNAL."
  (and (find-if (lambda (record)
                  (and (eq (journal-record-kind record) ':durable-mark)
                       (equal (getf (journal-record-payload record) ':marks)
                              record-id)))
                (journal-records journal))
       t))


(-> journal--form->record (journal t) journal-record)
(defun journal--form->record (journal form)
  "Build a record from a persisted FORM, validating its shape."
  (unless (and (listp form)
               (typep (getf form ':id) 'object-id)
               (typep (getf form ':timestamp) 'timestamp)
               (keywordp (getf form ':kind)))
    (error 'invalid-journal-record :journal journal :form form))
  (make-instance 'journal-record
                 :id (getf form ':id)
                 :timestamp (getf form ':timestamp)
                 :kind (getf form ':kind)
                 :payload (getf form ':payload)))
