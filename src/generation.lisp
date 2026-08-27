(in-package #:lispbsd)

;;;; -- Generation Metadata --

(defclass generation ()
  ((generation-id
    :initarg :id
    :initform (make-object-id)
    :reader generation-id
    :type object-id
    :documentation "Stable identifier of this generation.")
   (generation-world-id
    :initarg :world-id
    :reader generation-world-id
    :type object-id
    :documentation "Logical identity of the world this generation belongs to.")
   (generation-created-at
    :initarg :created-at
    :initform (current-timestamp)
    :reader generation-created-at
    :type timestamp
    :documentation "Universal time at which this generation was recorded.")
   (generation-parent-id
    :initarg :parent-id
    :initform nil
    :reader generation-parent-id
    :type (option object-id)
    :documentation "Parent generation identifier, if this is a successor.")
   (generation-runtime-name
    :initarg :runtime-name
    :reader generation-runtime-name
    :type string
    :documentation "Runtime name recorded in the generation manifest.")
   (generation-runtime-version
    :initarg :runtime-version
    :reader generation-runtime-version
    :type string
    :documentation "Runtime version recorded in the generation manifest.")
   (generation-source-revision
    :initarg :source-revision
    :initform nil
    :reader generation-source-revision
    :type (option string)
    :documentation "Tracked source revision, if known.")
   (generation-mutation-head
    :initarg :mutation-head
    :initform nil
    :reader generation-mutation-head
    :type (option object-id)
    :documentation "Journal record heading the mutation lineage, if any."))
  (:documentation
   "Named checkpoint metadata for a world, sufficient to validate it."))


(-> make-generation (object-id runtime &key (:parent-id (option object-id))
                                           (:source-revision (option string))
                                           (:mutation-head (option object-id)))
    generation)
(defun make-generation (world-id runtime &key parent-id source-revision
                        mutation-head)
  "Return generation metadata for WORLD-ID on RUNTIME."
  (make-instance 'generation
                 :world-id world-id
                 :runtime-name (runtime-name runtime)
                 :runtime-version (runtime-version runtime)
                 :parent-id parent-id
                 :source-revision source-revision
                 :mutation-head mutation-head))


(-> generation--plist (generation) list)
(defun generation--plist (generation)
  "Return a readable property list for GENERATION."
  (list :generation-id (generation-id generation)
        :world-id (generation-world-id generation)
        :created-at (generation-created-at generation)
        :parent-id (generation-parent-id generation)
        :runtime-name (generation-runtime-name generation)
        :runtime-version (generation-runtime-version generation)
        :source-revision (generation-source-revision generation)
        :mutation-head (generation-mutation-head generation)))


(-> generation--from-plist (list) generation)
(defun generation--from-plist (plist)
  "Reconstruct a generation from a readable PLIST."
  (unless (and (getf plist :generation-id)
               (getf plist :world-id)
               (getf plist :runtime-name)
               (getf plist :runtime-version))
    (error 'generation-error :path nil))
  (make-instance 'generation
                 :id (getf plist :generation-id)
                 :world-id (getf plist :world-id)
                 :created-at (or (getf plist :created-at) 0)
                 :parent-id (getf plist :parent-id)
                 :runtime-name (getf plist :runtime-name)
                 :runtime-version (getf plist :runtime-version)
                 :source-revision (getf plist :source-revision)
                 :mutation-head (getf plist :mutation-head)))


(-> generation-write (generation pathname) pathname)
(defun generation-write (generation path)
  "Write GENERATION's manifest to PATH and return PATH."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create)
    (let ((*print-readably* t)
          (*print-circle* t))
      (prin1 (generation--plist generation) stream)
      (terpri stream)))
  path)


(-> generation-read (pathname) generation)
(defun generation-read (path)
  "Read a generation manifest from PATH."
  (unless (probe-file path)
    (error 'generation-error :path path))
  (handler-case
      (with-open-file (stream path :direction ':input)
        (let* ((*read-eval* nil)
               (plist (read stream)))
          (generation--from-plist plist)))
    (generation-error (condition)
      (error condition))
    (error ()
      (error 'generation-error :path path))))
