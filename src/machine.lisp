(in-package #:lispbsd)

;;;; -- Machine Inventory --

(defclass machine ()
  ((machine-architecture
    :initarg :architecture
    :reader machine-architecture
    :type string
    :documentation "Processor architecture reported by the substrate.")
   (machine-processors
    :initarg :processors
    :reader machine-processors
    :type integer
    :documentation "Number of processors visible to the substrate.")
   (machine-memory
    :initarg :memory
    :reader machine-memory
    :type integer
    :documentation "Physical memory in bytes, or 0 if unknown.")
   (machine-resources
    :initarg :resources
    :initform nil
    :accessor machine-resources
    :documentation "Live machine resources attached to this object."))
  (:documentation
   "Hardware-facing object graph exposed to the world by the substrate."))


(-> read-file-line ((or string pathname)) (option string))
(defun read-file-line (path)
  "Return the first line of PATH, or NIL if PATH cannot be read."
  (handler-case
      (with-open-file (stream path :direction ':input)
        (let ((line (read-line stream nil nil)))
          (and line (string-trim '(#\Space #\Tab #\Newline) line))))
    (file-error ()
      nil)
    (error ()
      nil)))


(-> parse-meminfo-bytes (string) integer)
(defun parse-meminfo-bytes (line)
  "Parse a Linux meminfo LINE of the form NAME: COUNT kB into bytes."
  (let ((count (parse-integer line :start (position-if #'digit-char-p line)
                                   :junk-allowed t)))
    (* (or count 0) 1024)))


(-> machine--netbsd-p () boolean)
(defun machine--netbsd-p ()
  "Return true when this image runs on a NetBSD substrate."
  (and (string= (software-type) "NetBSD") t))


(-> machine--sysctl-integer (string) (option integer))
(defun machine--sysctl-integer (name)
  "Return the integer value of the NetBSD sysctl NAME, or NIL."
  (handler-case
      (let ((output (make-string-output-stream)))
        (sb-ext:run-program "/sbin/sysctl" (list "-n" name)
                            :output output
                            :wait t)
        (parse-integer (get-output-stream-string output) :junk-allowed t))
    (error ()
      nil)))


(-> hosted-processor-count () integer)
(defun hosted-processor-count ()
  "Return the number of processors visible on this host."
  (if (machine--netbsd-p)
      (or (machine--sysctl-integer "hw.ncpu") 1)
      (or (ignore-errors (count-if (lambda (line)
                                     (and (> (length line) 9)
                                          (string= line "processor" :end1 9)))
                                   (uiop:read-file-lines "/proc/cpuinfo")))
          1)))


(-> hosted-memory-bytes () integer)
(defun hosted-memory-bytes ()
  "Return host physical memory in bytes, or 0 if unknown."
  (if (machine--netbsd-p)
      (or (machine--sysctl-integer "hw.physmem64") 0)
      (handler-case
          (with-open-file (stream "/proc/meminfo" :direction ':input)
            (loop for line = (read-line stream nil nil)
                  while line
                  when (and (> (length line) 8)
                            (string= line "MemTotal" :end1 8))
                    return (parse-meminfo-bytes line)
                  finally (return 0)))
        (file-error ()
          0))))


(-> probe-hosted-network-interfaces () list)
(defun probe-hosted-network-interfaces ()
  "Return network-interface resources discovered on this host."
  (if (machine--netbsd-p)
      (probe-netbsd-network-interfaces)
      (let ((sys-class #p"/sys/class/net/"))
        (if (uiop:directory-exists-p sys-class)
            (mapcar (lambda (directory)
                      (let* ((name (first (last (pathname-directory directory))))
                             (address (read-file-line
                                       (merge-pathnames "address" directory)))
                             (operstate (read-file-line
                                         (merge-pathnames "operstate" directory))))
                        (make-instance 'network-interface
                                       :name name
                                       :address address
                                       :operstate operstate)))
                    (uiop:subdirectories sys-class))
            nil))))


(-> probe-hosted-machine () machine)
(defun probe-hosted-machine ()
  "Return a machine object describing the host this image is running on."
  (make-instance 'machine
                 :architecture (machine-type)
                 :processors (hosted-processor-count)
                 :memory (hosted-memory-bytes)
                 :resources (probe-hosted-network-interfaces)))
