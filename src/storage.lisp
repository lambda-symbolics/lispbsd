(in-package #:lispbsd)

;;;; -- Storage Volumes --

(defclass storage-volume (resource)
  ((storage-volume-device
    :initarg :device
    :reader storage-volume-device
    :type string
    :documentation "Device or source backing the volume.")
   (storage-volume-mount-point
    :initarg :mount-point
    :reader storage-volume-mount-point
    :type string
    :documentation "Directory where the volume is attached.")
   (storage-volume-filesystem
    :initarg :filesystem
    :reader storage-volume-filesystem
    :type string
    :documentation "Filesystem type name, for example \"ffs\".")
   (storage-volume-total-bytes
    :initarg :total-bytes
    :initform nil
    :reader storage-volume-total-bytes
    :type (option integer)
    :documentation "Volume capacity in bytes, if known.")
   (storage-volume-free-bytes
    :initarg :free-bytes
    :initform nil
    :reader storage-volume-free-bytes
    :type (option integer)
    :documentation "Bytes available to new data, if known."))
  (:default-initargs :kind ':storage-volume)
  (:documentation "A mounted filesystem volume."))


(-> probe-storage-volumes () list)
(defun probe-storage-volumes ()
  "Return storage-volume resources for the mounted filesystems here.

Only volumes backed by a device path are reported; pseudo filesystems
stay below the abstraction horizon."
  (let ((mounts (if (machine--netbsd-p)
                    (storage--netbsd-mounts)
                    (storage--linux-mounts))))
    (loop for (device mount-point filesystem) in mounts
          collect (multiple-value-bind (total free)
                      (storage--df-bytes mount-point)
                    (make-instance 'storage-volume
                                   :name mount-point
                                   :device device
                                   :mount-point mount-point
                                   :filesystem filesystem
                                   :total-bytes total
                                   :free-bytes free)))))


(-> storage--linux-mounts () list)
(defun storage--linux-mounts ()
  "Return (device mount-point filesystem) lists from /proc/mounts."
  (handler-case
      (loop for line in (uiop:read-file-lines "/proc/mounts")
            for parsed = (storage--parse-proc-mounts-line line)
            when parsed
              collect parsed)
    (error ()
      nil)))


(-> storage--netbsd-mounts () list)
(defun storage--netbsd-mounts ()
  "Return (device mount-point filesystem) lists from NetBSD mount."
  (handler-case
      (let ((output (make-string-output-stream)))
        (sb-ext:run-program "/sbin/mount" nil
                            :output output
                            :wait t)
        (loop for line in (uiop:split-string
                           (get-output-stream-string output)
                           :separator '(#\Newline))
              for parsed = (storage--parse-netbsd-mount-line line)
              when parsed
                collect parsed))
    (error ()
      nil)))


(-> storage--parse-proc-mounts-line (string) (option list))
(defun storage--parse-proc-mounts-line (line)
  "Parse a /proc/mounts LINE into (device mount-point filesystem), or NIL.

Only device-backed mounts are reported."
  (let ((fields (uiop:split-string line :separator '(#\Space))))
    (when (and (>= (length fields) 3)
               (>= (length (first fields)) 5)
               (string= "/dev/" (first fields) :end2 5))
      (list (first fields) (second fields) (third fields)))))


(-> storage--parse-netbsd-mount-line (string) (option list))
(defun storage--parse-netbsd-mount-line (line)
  "Parse a NetBSD mount output LINE into (device mount-point filesystem).

Lines look like \"/dev/wd0a on / type ffs (local)\". Only device-backed
mounts are reported."
  (block nil
    (let ((on (search " on " line))
          (type-marker (search " type " line)))
      (unless (and on type-marker (< on type-marker))
        (return nil))
      (let ((device (subseq line 0 on))
            (mount-point (subseq line (+ on 4) type-marker))
            (rest (subseq line (+ type-marker 6))))
        (unless (and (>= (length device) 5)
                     (string= "/dev/" device :end2 5))
          (return nil))
        (let ((space (position #\Space rest)))
          (list device
                mount-point
                (subseq rest 0 (or space (length rest)))))))))


(-> storage--df-bytes (string) (values (option integer) (option integer)))
(defun storage--df-bytes (mount-point)
  "Return total and free bytes for MOUNT-POINT via df, or NILs."
  (handler-case
      (let ((output (make-string-output-stream)))
        (sb-ext:run-program "df" (list "-Pk" mount-point)
                            :output output
                            :search t
                            :wait t)
        (let ((lines (uiop:split-string (get-output-stream-string output)
                                        :separator '(#\Newline))))
          (storage--parse-df-line (second lines))))
    (error ()
      (values nil nil))))


(-> storage--parse-df-line ((option string))
    (values (option integer) (option integer)))
(defun storage--parse-df-line (line)
  "Return total and free bytes from a POSIX df -Pk data LINE, or NILs."
  (block nil
    (unless line
      (return (values nil nil)))
    (let ((fields (remove "" (uiop:split-string line :separator '(#\Space))
                          :test #'string=)))
      (unless (>= (length fields) 4)
        (return (values nil nil)))
      (let ((blocks (parse-integer (second fields) :junk-allowed t))
            (available (parse-integer (fourth fields) :junk-allowed t)))
        (if (and blocks available)
            (values (* blocks 1024) (* available 1024))
            (values nil nil))))))
