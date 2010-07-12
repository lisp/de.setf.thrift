;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: thrift; -*-

(in-package :thrift)

;;; test for thrift client/server operators

;;; loads test services from the thrift release, starts a server and exercises each one.
;;; this operates on the content of #P"THRIFT:..;tests;cl-gen"'. in order to prepare the
;;; service desription
;;;
;;;  $ cd $THRIFT/test/
;;;  $ ls *.thrift | while read file; do thrift --gen cl $file; done
;;;
;;; Make adjustments as appropriate to #P"THRIFT:test;test.asd" to account for test focus.
;;; Load the test system, of which this file startes a server and exercises the services.
;;;
;;;  > (asdf:load-system :org.apache.thrift.test)




(test 


(defclass service ()
  ((base-services
    :initform nil :initarg :base-services
    :reader service-base-services)
   (methods
    :reader service-methods)))


(defclass server ()
  ((trans :accessor server-trans :initarg :trans)
   (services
    :initform nil :initarg :services
    :accessor server-services
    :documentation "A sequence of services, each of which binds its set of operators
     to external names. When accepting a message, the server locates some service which
     can respond toit, and delegates the processing to that service. If none is found
     an exception is returned.")))

(defclass socket-server (server)
  ())


;;;
;;; service operators

(defmethod initialize-instance :after ((instance service) &key methods)
  (etypecase methods
    (hash-table (setf (slot-value instance 'methods) methods))
    (list (setf (slot-value instance 'methods)
                (thrift:map methods)))))

(defgeneric find-thrift-function (service identifier)
  (:method ((service service) (identifier string))
    (flet ((delegate-find (service) (find-thrift-function service identifier)))
      (declare (dynamic-extent #'delegate-find))
      (or (gethash identifier (service-methods service))
          (some #'delegate-find (service-base-services service))))))

(defgeneric (setf find-thrift-function) (function service identifier)
  (:method ((function thrift-generic-function) (service service) (identifier string))
    (setf (gethash identifier (service-methods service)) function))
  (:method ((function null) (service service) (identifier string))
    (remhash identifier (service-methods service))))


;;;
;;; server operators

(defgeneric server-input-transport (server connection)
  (:method ((server socket-server) (socket usocket:socket))
    (make-instance 'socket-transport :socket socket)))

(defgeneric server-output-transport (server connection)
  (:method ((server socket-server) (socket usocket:socket))
    (make-instance 'socket-transport :socket socket)))
    

(defmethod accept-connection ((s socket-server))
  (usocket:socket-accept (server-trans s)))

(defmethod server-close ((s socket-server))
  (usocket:socket-close (server-trans s)))

(defgeneric server-protocol (server input output)
  (:method ((server socket-server) input output)
    (make-instance 'binary-protocol :input-transport input :output-transport output)))


(defgeneric serve (connection-server service)
  (:documentation "Accept to a CONNECTION-SERVER, configure the CLIENT's transport and protocol
 in combination with the connection, and process messages until the connection closes.")

  (:method ((location puri:uri) service)
    (let ((server (make-instance 'socket-server
                    :trans (usocket:socket-listen (puri:uri-host location) (puri:uri-port location)
                                                  :reuseaddress t))))
      (unwind-protect (serve server service)
        (server-close server))))

  (:method ((s server) (service service))
    (loop 
      (let* ((connection (accept-connection s))
             (input-transport (server-input-transport s connection))
             (output-transport (server-output-transport s connection))
             (protocol (server-protocol s input-transport output-transport)))
        (loop (handler-case (progn (process service protocol)
                                   (unless (open-stream-p connection) (return)))
                (error (error)
                       (warn "Server error: ~s: ~a" s error)
                       (stream-write-exception protocol error)
                       (return))))
        (close input-transport)
        (close output-transport)))))
  
(defgeneric process (service protocol)
  (:documentation "Combine a service PEER with an input-protocol and an output-protocol to control processing
 the next message on the peer's input connection. The base method reads the message, decodes the
 function and the arguments, invokes the method, and replies with the results.
 The protocols are initially those of the peer itself, but they are passed her in order to permit
 wrapping for logging, etc.")

  (:method ((service service) (protocol t))
    (flet ((consume-message ()
             (prog1 (stream-read-struct protocol)
               (stream-read-message-end protocol))))
      (multiple-value-bind (identifier type sequence-number) (stream-read-message-begin protocol)
        (ecase type
          ((call oneway)
           (let ((operator (find-thrift-function service identifier)))
             (cond (operator
                    (funcall operator service sequence-number protocol)
                    (stream-read-message-end protocol))
                   (t
                    (method-missing protocol identifier sequence-number (consume-message))))))
          (reply
           (unexpected-reply protocol identifier sequence-number (consume-message)))
          (exception
           (request-exception protocol identifier sequence-number (consume-message))))))))


