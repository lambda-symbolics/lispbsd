(in-package #:lispbsd)

;;;; -- Checkpoints and the Generation Registry --

(defclass generation-store ()
  ((generation-store-directory
    :initarg :directory
    :reader generation-store-directory
    :documentation "Directory holding manifests and the registry.")
   (generation-store-registry
    :initarg :registry
    :reader generation-store-registry
    :documentation "Append-only journal recording each retained generation."))
  (:documentation
   "Durable storage for generation manifests and their registry.

The registry is simple enough to inspect and repair without loading
any world."))


(-> make-generation-store (&key (:directory (or pathname string)))
    generation-store)
(defun make-generation-store (&key directory)
  "Return a generation store rooted at DIRECTORY."
  (let ((root (uiop:ensure-directory-pathname directory)))
    (make-instance 'generation-store
                   :directory root
                   :registry (make-journal
                              :path (merge-pathnames "registry.lisp" root)))))


(-> generation-store-manifest-path (generation-store object-id) pathname)
(defun generation-store-manifest-path (store generation-id)
  "Return the manifest path for GENERATION-ID in STORE."
  (merge-pathnames (format nil "~A.lisp" generation-id)
                   (generation-store-directory store)))


(-> generation-store-record (generation-store generation) generation)
(defun generation-store-record (store generation)
  "Write GENERATION's manifest into STORE and register it."
  (generation-write generation
                    (generation-store-manifest-path
                     store (generation-id generation)))
  (journal-append (generation-store-registry store)
                  ':generation
                  (generation--plist generation))
  generation)


(-> generation-store-generations (generation-store) list)
(defun generation-store-generations (store)
  "Return the retained generations in STORE, oldest first.

Reads only the registry, so it works without any world loaded."
  (loop for record in (journal-records (generation-store-registry store))
        when (eq (journal-record-kind record) ':generation)
          collect (generation--from-plist (journal-record-payload record))))


(-> generation-store-find (generation-store object-id) (option generation))
(defun generation-store-find (store generation-id)
  "Return the generation with GENERATION-ID from STORE, or NIL."
  (find generation-id (generation-store-generations store)
        :key #'generation-id
        :test #'equal))


(-> world-checkpoint (world generation-store &key (:note (option string)))
    generation)
(defun world-checkpoint (world store &key note)
  "Record WORLD's current state as a new retained generation.

A successor generation is created, written to STORE, journaled when
the world keeps a journal, and selected as the world's current
generation. Returns it."
  (let* ((current (world-generation world))
         (generation (make-generation
                      (world-id world)
                      (world-runtime world)
                      :parent-id (and current (generation-id current))
                      :source-revision (and current
                                            (generation-source-revision
                                             current))
                      :mutation-head (and current
                                          (generation-mutation-head
                                           current)))))
    (generation-store-record store generation)
    (setf (world-generation world) generation)
    (let ((journal (world-journal world)))
      (when journal
        (journal-append journal ':checkpoint
                        (list ':generation-id (generation-id generation)
                              ':note note))))
    (emit-event (world-history world)
                ':world-checkpointed
                :source world
                :payload (list :generation-id (generation-id generation)
                               :note note))
    generation))
