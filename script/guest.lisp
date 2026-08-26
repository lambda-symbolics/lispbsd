;;;; Host-side driver for a LispBSD QEMU guest serial console.
;;;;
;;;; Usage: script/guest command...
;;;;        script/guest -c 'shell snippet'

(require "asdf")
(require "sb-bsd-sockets")

(defparameter *guest-prompt* "LISPBSD# "
  "Prompt installed after login so command output can be framed.")

(defparameter *default-kernel*
  "/root/common-lisp/refs/netbsd-obj/sys/arch/amd64/compile/LISPBSD/netbsd"
  "Default Multiboot kernel built by script/netbsd-build.")

(define-condition guest-error (error)
  ((guest-error-message
    :initarg :message
    :reader guest-error-message))
  (:report (lambda (condition stream)
             (write-string (guest-error-message condition) stream))))

(defun guest-argv ()
  "Return user arguments after sbcl --script guest.lisp."
  (let ((args sb-ext:*posix-argv*))
    (when (and args (search "sbcl" (first args)))
      (setf args (rest args)))
    (when (and args (string= (first args) "--script"))
      (setf args (rest args)))
    (when (and args (search "guest.lisp" (first args)))
      (setf args (rest args)))
    args))

(defun guest-repository-root ()
  "Return the LispBSD repository root containing this script."
  (let ((script (or *load-truename* *load-pathname*)))
    (unless script
      (error 'guest-error :message "cannot locate guest.lisp"))
    (truename (merge-pathnames "../" (make-pathname :name nil :type nil
                                                    :defaults script)))))

(defun guest-getenv (name &optional default)
  "Return environment variable NAME, or DEFAULT when unset or empty."
  (let ((value (uiop:getenv name)))
    (if (and value (plusp (length value)))
        value
        default)))

(defun guest-parse-arguments (arguments)
  "Parse ARGUMENTS into a guest shell command string."
  (cond
    ((null arguments)
     (let ((text (string-trim '(#\Space #\Tab #\Newline)
                              (uiop:slurp-stream-string *standard-input*))))
       (when (zerop (length text))
         (error 'guest-error :message "usage: script/guest command..."))
       text))
    ((string= (first arguments) "-c")
     (unless (rest arguments)
       (error 'guest-error :message "usage: script/guest -c command"))
     (format nil "~{~A~^ ~}" (rest arguments)))
    (t
     (format nil "~{~A~^ ~}" arguments))))

(defun guest-host-address (host)
  "Return the IPv4 address vector for HOST."
  (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host)))

(defun guest-open-serial (host port &key (attempts 40))
  "Connect to the QEMU serial TCP port, retrying until it listens."
  (loop for attempt from 1 to attempts do
    (handler-case
        (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                     :type :stream :protocol :tcp)))
          (sb-bsd-sockets:socket-connect socket (guest-host-address host) port)
          (return (sb-bsd-sockets:socket-make-stream socket
                                                     :input t
                                                     :output t
                                                     :element-type 'character
                                                     :buffering :none)))
      (error (condition)
        (when (= attempt attempts)
          (error 'guest-error
                 :message (format nil "cannot connect to ~A:~A: ~A"
                                  host port condition)))
        (sleep 0.5)))))

(defun guest-send (stream string)
  "Write STRING to STREAM followed by a carriage return."
  (write-string string stream)
  (write-char #\Return stream)
  (force-output stream))

(defun guest-read (stream timeout)
  "Read available characters from STREAM, waiting up to TIMEOUT seconds."
  (let ((fd (sb-sys:fd-stream-fd stream)))
    (when (sb-sys:wait-until-fd-usable fd :input timeout)
      (loop while (listen stream)
            for character = (read-char-no-hang stream nil nil)
            while character
            collect character into characters
            finally (return (coerce characters 'string))))))

(defun guest-wait-for-any (stream patterns timeout)
  "Accumulate STREAM output until one of PATTERNS appears."
  (let ((buffer (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (deadline (+ (get-internal-real-time)
                     (floor (* timeout internal-time-units-per-second)))))
    (loop
      (let ((remaining (/ (- deadline (get-internal-real-time))
                          internal-time-units-per-second)))
        (when (<= remaining 0)
          (error 'guest-error
                 :message (format nil "timed out waiting for ~S; saw:~%~A"
                                  patterns buffer)))
        (let ((chunk (guest-read stream (max 0.1 (min 1 remaining)))))
          (when chunk
            (loop for character across chunk do
              (vector-push-extend character buffer))
            (when (find-if (lambda (pattern)
                             (search pattern buffer))
                           patterns)
              (return (copy-seq buffer)))))))))

(defun guest-wait-for (stream pattern timeout)
  "Accumulate STREAM output until PATTERN appears."
  (guest-wait-for-any stream (list pattern) timeout))

(defun guest-start-qemu (image kernel memory host port)
  "Start QEMU for IMAGE and return the process."
  (let* ((root (guest-repository-root))
         (vm-run (merge-pathnames "script/vm-run" root))
         (pkg-disk (guest-getenv "PKG_DISK"))
         (environment (append (list (format nil "MEMORY=~A" memory)
                                    (format nil "SERIAL=tcp:~A:~A,server,nowait"
                                            host port))
                              (when (and kernel (probe-file kernel))
                                (list (format nil "KERNEL=~A"
                                              (namestring (truename kernel)))))
                              (sb-ext:posix-environ)))
         (command (append (list (namestring vm-run) (namestring image))
                          (when (and pkg-disk (probe-file pkg-disk))
                            (list "-drive"
                                  (format nil "file=~A,format=raw,index=1"
                                          (namestring (truename pkg-disk))))))))
    (uiop:launch-program command
                         :environment environment
                         :output (merge-pathnames "vm/qemu-guest.log" root)
                         :error-output :output)))

(defun guest-login (stream)
  "Reach a root shell on STREAM and install *GUEST-PROMPT*."
  (guest-wait-for stream "login:" 120)
  (guest-send stream "root")
  (let ((text (guest-wait-for-any stream '("Password:" "# " "$ ") 30)))
    (when (search "Password:" text)
      (guest-send stream "")
      (guest-wait-for-any stream '("# " "$ ") 30)))
  (guest-send stream (format nil "export PS1='~A'" *guest-prompt*))
  (guest-wait-for stream *guest-prompt* 15)
  t)

(defun guest-run-command (stream command &key (timeout 300))
  "Run COMMAND at the guest prompt and return the output after the echo."
  (guest-send stream command)
  (let* ((text (guest-wait-for stream *guest-prompt* timeout))
         (echo-end (let ((cr (position #\Return text)))
                     (if cr (1+ cr) 0)))
         (prompt-start (search *guest-prompt* text :from-end t)))
    (string-trim '(#\Space #\Tab #\Return #\Newline)
                 (subseq text echo-end (or prompt-start (length text))))))

(defun guest-shutdown (stream process)
  "Halt the guest and wait for QEMU to exit."
  (ignore-errors (guest-send stream "shutdown -p now"))
  (let ((deadline (+ (get-internal-real-time)
                     (* 30 internal-time-units-per-second))))
    (loop until (not (uiop:process-alive-p process))
          do (when (>= (get-internal-real-time) deadline)
               (uiop:terminate-process process :urgent t)
               (return))
             (sleep 0.5)))
  (ignore-errors (uiop:wait-process process))
  t)

(defun guest-main (&optional (arguments (guest-argv)))
  "Boot the guest, run the requested command, and halt."
  (let* ((command (guest-parse-arguments arguments))
         (root (guest-repository-root))
         (image (pathname (guest-getenv "GUEST_IMAGE"
                                        (namestring (merge-pathnames
                                                     "vm/lispbsd-live.img"
                                                     root)))))
         (kernel (guest-getenv "KERNEL" *default-kernel*))
         (memory (guest-getenv "MEMORY" "2048"))
         (host (guest-getenv "SERIAL_HOST" "127.0.0.1"))
         (port (parse-integer (guest-getenv "SERIAL_PORT" "4555")))
         (process nil)
         (stream nil))
    (unless (probe-file image)
      (error 'guest-error :message (format nil "missing image ~A" image)))
    (unwind-protect
         (progn
           (setf process (guest-start-qemu image kernel memory host port))
           (setf stream (guest-open-serial host port))
           (guest-login stream)
           (let ((output (guest-run-command stream command :timeout 600)))
             (write-string output)
             (terpri)
             (guest-shutdown stream process)
             (setf process nil)
             output))
      (when stream
        (ignore-errors (close stream)))
      (when (and process (uiop:process-alive-p process))
        (uiop:terminate-process process :urgent t)
        (ignore-errors (uiop:wait-process process))))))

(handler-case
    (guest-main)
  (guest-error (condition)
    (format *error-output* "~A~%" condition)
    (uiop:quit 1))
  (error (condition)
    (format *error-output* "guest failed: ~A~%" condition)
    (uiop:quit 1)))
