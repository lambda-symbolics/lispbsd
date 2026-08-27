(in-package #:lispbsd)

;;;; -- Network Control --

(defvar *network-substrate* nil
  "The active network control substrate, or NIL before machine attach.")


(define-condition network-error (lispbsd-error)
  ((network-error-interface
    :initarg :interface
    :initform nil
    :reader network-error-interface
    :documentation "The interface involved in the failure, if any."))
  (:documentation "A failure involving network control."))


(define-condition network-operation-unsupported (network-error)
  ((network-operation-unsupported-operation
    :initarg :operation
    :reader network-operation-unsupported-operation
    :documentation "The operation the substrate cannot perform."))
  (:report (lambda (condition stream)
             (format stream "Network operation ~S is not supported here."
                     (network-operation-unsupported-operation condition))))
  (:documentation "The active substrate cannot perform an operation."))


(define-condition network-operation-failed (network-error)
  ((network-operation-failed-operation
    :initarg :operation
    :reader network-operation-failed-operation
    :documentation "The operation that failed.")
   (network-operation-failed-detail
    :initarg :detail
    :initform nil
    :reader network-operation-failed-detail
    :documentation "Substrate error text, if any."))
  (:report (lambda (condition stream)
             (format stream "Network operation ~S failed~@[: ~A~]."
                     (network-operation-failed-operation condition)
                     (network-operation-failed-detail condition))))
  (:documentation "The substrate rejected or could not complete an operation."))


(defclass network-address ()
  ((network-address-value
    :initarg :value
    :reader network-address-value
    :type string
    :documentation "Literal address text, for example \"10.0.2.15\".")
   (network-address-family
    :initarg :family
    :reader network-address-family
    :documentation "Address family, ':ipv4 or ':ipv6.")
   (network-address-prefix-length
    :initarg :prefix-length
    :initform nil
    :reader network-address-prefix-length
    :type (option integer)
    :documentation "Prefix length in bits, if known."))
  (:documentation "An address assigned to a network interface."))


(-> make-network-address (&key (:value string) (:family keyword)
                              (:prefix-length (option integer)))
    network-address)
(defun make-network-address (&key value (family ':ipv4) prefix-length)
  "Return an address object for VALUE in FAMILY."
  (make-instance 'network-address
                 :value value
                 :family family
                 :prefix-length prefix-length))


(defclass network-substrate ()
  ()
  (:documentation
   "Adapter performing network operations on the underlying system."))


(defgeneric network-substrate-interface-state (substrate interface)
  (:documentation "Return ':up, ':down, or ':unknown for INTERFACE."))

(defgeneric network-substrate-set-interface-state (substrate interface state)
  (:documentation "Bring INTERFACE to STATE, ':up or ':down."))

(defgeneric network-substrate-interface-addresses (substrate interface)
  (:documentation "Return the network-address objects assigned to INTERFACE."))

(defgeneric network-substrate-add-interface-address (substrate interface address)
  (:documentation "Assign ADDRESS to INTERFACE."))

(defgeneric network-substrate-remove-interface-address (substrate interface address)
  (:documentation "Remove ADDRESS from INTERFACE."))


;;; Semantic interface operations

(-> interface-state (network-interface) keyword)
(defun interface-state (interface)
  "Return ':up, ':down, or ':unknown for INTERFACE."
  (network-substrate-interface-state (network--substrate ':interface-state
                                                          interface)
                                     interface))


(-> interface-up (network-interface) network-interface)
(defun interface-up (interface)
  "Bring INTERFACE up. Returns INTERFACE."
  (network-substrate-set-interface-state (network--substrate ':interface-up
                                                              interface)
                                         interface ':up)
  (network--record interface ':interface-up)
  interface)


(-> interface-down (network-interface) network-interface)
(defun interface-down (interface)
  "Bring INTERFACE down. Returns INTERFACE."
  (network-substrate-set-interface-state (network--substrate ':interface-down
                                                              interface)
                                         interface ':down)
  (network--record interface ':interface-down)
  interface)


(-> interface-addresses (network-interface) list)
(defun interface-addresses (interface)
  "Return the network-address objects assigned to INTERFACE."
  (network-substrate-interface-addresses (network--substrate
                                          ':interface-addresses
                                          interface)
                                         interface))


(-> interface-add-address (network-interface network-address) network-interface)
(defun interface-add-address (interface address)
  "Assign ADDRESS to INTERFACE. Returns INTERFACE."
  (network-substrate-add-interface-address (network--substrate
                                            ':interface-add-address
                                            interface)
                                           interface address)
  (network--record interface ':interface-add-address)
  interface)


(-> interface-remove-address (network-interface network-address)
    network-interface)
(defun interface-remove-address (interface address)
  "Remove ADDRESS from INTERFACE. Returns INTERFACE."
  (network-substrate-remove-interface-address (network--substrate
                                               ':interface-remove-address
                                               interface)
                                              interface address)
  (network--record interface ':interface-remove-address)
  interface)


(defmethod resource-operations ((resource network-interface))
  (list (make-operation :name ':interface-up
                        :label "Bring Up"
                        :function #'interface-up)
        (make-operation :name ':interface-down
                        :label "Bring Down"
                        :function #'interface-down)))


(-> probe-network-substrate () network-substrate)
(defun probe-network-substrate ()
  "Return the network substrate matching this host."
  (if (string= (software-type) "NetBSD")
      (make-instance 'netbsd-network-substrate)
      (make-instance 'linux-network-substrate)))


(-> network--substrate (keyword network-interface) network-substrate)
(defun network--substrate (operation interface)
  "Return the active substrate or signal that OPERATION is unsupported."
  (or *network-substrate*
      (error 'network-operation-unsupported
             :interface interface
             :operation operation)))


(-> network--record (network-interface keyword) t)
(defun network--record (interface operation)
  "Emit a configuration event for OPERATION on INTERFACE."
  (when *world*
    (emit-event (world-history *world*)
                ':interface-configured
                :source interface
                :payload (list :interface (resource-name interface)
                               :operation operation)))
  nil)


;;; Hosted Linux substrate: inspection only

(defclass linux-network-substrate (network-substrate)
  ()
  (:documentation
   "Read-only substrate for hosted development on Linux.

State is read from sysfs; the development host is never mutated."))


(defmethod network-substrate-interface-state ((substrate linux-network-substrate)
                                              interface)
  (let ((operstate (read-file-line (format nil "/sys/class/net/~A/operstate"
                                           (resource-name interface)))))
    (cond ((equal operstate "up")
           ':up)
          ((equal operstate "down")
           ':down)
          (t
           ':unknown))))

(defmethod network-substrate-set-interface-state ((substrate linux-network-substrate)
                                                  interface state)
  (declare (ignore state))
  (error 'network-operation-unsupported
         :interface interface
         :operation ':set-interface-state))

(defmethod network-substrate-interface-addresses ((substrate linux-network-substrate)
                                                  interface)
  (declare (ignore interface))
  nil)

(defmethod network-substrate-add-interface-address ((substrate linux-network-substrate)
                                                    interface address)
  (declare (ignore address))
  (error 'network-operation-unsupported
         :interface interface
         :operation ':interface-add-address))

(defmethod network-substrate-remove-interface-address ((substrate linux-network-substrate)
                                                       interface address)
  (declare (ignore address))
  (error 'network-operation-unsupported
         :interface interface
         :operation ':interface-remove-address))


;;; NetBSD substrate: ifconfig beneath the protocol

(defclass netbsd-network-substrate (network-substrate)
  ()
  (:documentation
   "Substrate driving NetBSD interface configuration.

The mechanism underneath is /sbin/ifconfig; parsing stays confined to
this adapter and the protocol exposes typed objects."))


(defmethod network-substrate-interface-state ((substrate netbsd-network-substrate)
                                              interface)
  (let ((output (netbsd-network--ifconfig
                 (list (resource-name interface))
                 ':interface-state interface)))
    (if (netbsd-network--flags-up-p output)
        ':up
        ':down)))

(defmethod network-substrate-set-interface-state ((substrate netbsd-network-substrate)
                                                  interface state)
  (netbsd-network--ifconfig (list (resource-name interface)
                                  (ecase state
                                    (:up "up")
                                    (:down "down")))
                            ':set-interface-state interface)
  state)

(defmethod network-substrate-interface-addresses ((substrate netbsd-network-substrate)
                                                  interface)
  (let ((output (netbsd-network--ifconfig
                 (list (resource-name interface))
                 ':interface-addresses interface)))
    (loop for line in (uiop:split-string output :separator '(#\Newline))
          for address = (netbsd-network--parse-address-line line)
          when address
            collect address)))

(defmethod network-substrate-add-interface-address ((substrate netbsd-network-substrate)
                                                    interface address)
  (netbsd-network--ifconfig (append (list (resource-name interface))
                                    (netbsd-network--address-arguments address)
                                    (list "alias"))
                            ':interface-add-address interface)
  address)

(defmethod network-substrate-remove-interface-address ((substrate netbsd-network-substrate)
                                                       interface address)
  (netbsd-network--ifconfig (append (list (resource-name interface))
                                    (netbsd-network--address-arguments address)
                                    (list "-alias"))
                            ':interface-remove-address interface)
  address)


(-> netbsd-network--address-arguments (network-address) list)
(defun netbsd-network--address-arguments (address)
  "Return the ifconfig arguments naming ADDRESS."
  (list (ecase (network-address-family address)
          (:ipv4 "inet")
          (:ipv6 "inet6"))
        (if (network-address-prefix-length address)
            (format nil "~A/~A"
                    (network-address-value address)
                    (network-address-prefix-length address))
            (network-address-value address))))


(-> probe-netbsd-network-interfaces () list)
(defun probe-netbsd-network-interfaces ()
  "Return network-interface resources discovered via NetBSD ifconfig."
  (handler-case
      (let ((output (make-string-output-stream)))
        (sb-ext:run-program "/sbin/ifconfig" (list "-l")
                            :output output
                            :wait t)
        (loop for name in (uiop:split-string
                           (string-trim '(#\Space #\Newline)
                                        (get-output-stream-string output)))
              when (plusp (length name))
                collect (let ((detail (netbsd-network--ifconfig (list name)
                                                                ':probe nil)))
                          (make-instance 'network-interface
                                         :name name
                                         :address (netbsd-network--parse-hardware-address
                                                   detail)
                                         :operstate (if (netbsd-network--flags-up-p detail)
                                                        "up"
                                                        "down")))))
    (error ()
      nil)))


(-> netbsd-network--parse-hardware-address (string) (option string))
(defun netbsd-network--parse-hardware-address (output)
  "Return the hardware address reported in ifconfig OUTPUT, or NIL."
  (let ((start (search "address: " output)))
    (when start
      (let ((value-start (+ start (length "address: "))))
        (string-trim '(#\Space #\Tab)
                     (subseq output value-start
                             (position #\Newline output :start value-start)))))))


(-> netbsd-network--ifconfig (list keyword (option network-interface)) string)
(defun netbsd-network--ifconfig (arguments operation interface)
  "Run /sbin/ifconfig with ARGUMENTS and return its output.

Signals NETWORK-OPERATION-FAILED with the error text when ifconfig
exits nonzero."
  (let ((output (make-string-output-stream))
        (errors (make-string-output-stream)))
    (let ((exit-code (sb-ext:process-exit-code
                      (sb-ext:run-program "/sbin/ifconfig" arguments
                                          :output output
                                          :error errors
                                          :wait t))))
      (unless (zerop exit-code)
        (error 'network-operation-failed
               :interface interface
               :operation operation
               :detail (string-trim '(#\Space #\Newline)
                                    (get-output-stream-string errors))))
      (get-output-stream-string output))))


(-> netbsd-network--flags-up-p (string) boolean)
(defun netbsd-network--flags-up-p (output)
  "Return true when ifconfig OUTPUT reports the UP interface flag."
  (let ((open (search "flags=" output)))
    (and open
         (let* ((angle-start (position #\< output :start open))
                (angle-end (and angle-start
                                (position #\> output :start angle-start))))
           (and angle-start
                angle-end
                (let ((flags (subseq output (1+ angle-start) angle-end)))
                  (and (member "UP" (uiop:split-string flags :separator '(#\,))
                               :test #'string=)
                       t)))))))


(-> netbsd-network--parse-address-line (string) (option network-address))
(defun netbsd-network--parse-address-line (line)
  "Parse an ifconfig inet or inet6 LINE into a network-address, or NIL."
  (block nil
    (let* ((trimmed (string-trim '(#\Space #\Tab) line))
           (space (position #\Space trimmed)))
      (unless space
        (return nil))
      (let ((head (subseq trimmed 0 space)))
        (unless (or (string= head "inet") (string= head "inet6"))
          (return nil))
        (let* ((tail (string-left-trim '(#\Space) (subseq trimmed space)))
               (end (or (position #\Space tail) (length tail)))
               (token (subseq tail 0 end))
               (slash (position #\/ token))
               (value (subseq token 0 (or slash (length token))))
               (prefix (and slash
                            (parse-integer token :start (1+ slash)
                                                 :junk-allowed t)))
               (zone (position #\% value)))
          (make-instance 'network-address
                         :value (if zone
                                    (subseq value 0 zone)
                                    value)
                         :family (if (string= head "inet")
                                     ':ipv4
                                     ':ipv6)
                         :prefix-length prefix))))))
