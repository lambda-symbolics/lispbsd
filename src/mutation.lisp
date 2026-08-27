(in-package #:lispbsd)

;;;; -- Durable World Mutations --

(define-condition world-journal-missing (world-error)
  ()
  (:report (lambda (condition stream)
             (format stream "World ~S has no mutation journal."
                     (world-error-world condition))))
  (:documentation "A durable mutation was requested of an ephemeral world."))


(define-condition world-mutation-error (world-error)
  ((world-mutation-error-record
    :initarg :record
    :initform nil
    :reader world-mutation-error-record
    :documentation "The journaled intent record of the failed mutation.")
   (world-mutation-error-underlying
    :initarg :underlying
    :initform nil
    :reader world-mutation-error-underlying
    :documentation "The condition that stopped the mutation."))
  (:report (lambda (condition stream)
             (format stream "Durable mutation failed: ~A."
                     (world-mutation-error-underlying condition))))
  (:documentation
   "A durable definition mutation failed before its durable mark."))


(-> world-mutate-definition (world t &key (:check (option function))
                                   (:note (option string)))
    journal-record)
(defun world-mutate-definition (world form &key check note)
  "Durably install the definition FORM in WORLD.

Preserves the durable mutation order: the intent is journaled, FORM is
compiled and installed through the world's runtime, CHECK runs when
supplied, a successor generation is recorded carrying the mutation
lineage, and the journal entry is marked durable. Returns the intent
record.

A failure after installation signals WORLD-MUTATION-ERROR and does not
roll back the live definition; the intent record then stays without a
durable mark, so journal replay excludes the failed mutation."
  (let ((journal (world-journal world)))
    (unless journal
      (error 'world-journal-missing :world world))
    (let ((record (journal-append journal ':definition-mutation
                                  (list ':form form ':note note))))
      (handler-case
          (progn
            (runtime-install-definition (world-runtime world) form)
            (when check
              (funcall check))
            (setf (world-generation world)
                  (make-generation (world-id world)
                                   (world-runtime world)
                                   :parent-id (and (world-generation world)
                                                   (generation-id
                                                    (world-generation world)))
                                   :mutation-head (journal-record-id record)))
            (journal-mark-durable journal (journal-record-id record))
            (emit-event (world-history world)
                        ':definition-mutated
                        :source world
                        :payload (list :record-id (journal-record-id record)
                                       :note note)))
        (error (condition)
          (emit-event (world-history world)
                      ':definition-mutation-failed
                      :source world
                      :payload (list :record-id (journal-record-id record)))
          (error 'world-mutation-error
                 :world world
                 :record record
                 :underlying condition)))
      record)))


(-> journal-replay (journal runtime) integer)
(defun journal-replay (journal runtime)
  "Reinstall JOURNAL's durable definition mutations through RUNTIME.

Only records with a durability mark are replayed, in append order.
Returns the number of definitions reinstalled."
  (let* ((records (journal-records journal))
         (durable-ids (loop for record in records
                            when (eq (journal-record-kind record)
                                     ':durable-mark)
                              collect (getf (journal-record-payload record)
                                            ':marks)))
         (replayed 0))
    (dolist (record records)
      (when (and (eq (journal-record-kind record) ':definition-mutation)
                 (member (journal-record-id record) durable-ids
                         :test #'equal))
        (runtime-install-definition runtime
                                    (getf (journal-record-payload record)
                                          ':form))
        (incf replayed)))
    replayed))


(-> world-replay-journal (world) integer)
(defun world-replay-journal (world)
  "Reinstall WORLD's durable definition mutations in append order."
  (let ((journal (world-journal world)))
    (unless journal
      (error 'world-journal-missing :world world))
    (journal-replay journal (world-runtime world))))
