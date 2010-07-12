;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: thrift-test; -*-

(in-package :thrift-test)

;;; define a binary stream to wrap a vector for use in tests.
;;; adapted from the cl-xml version to restrict i/o to unsigned byte operations.
;;; this version uses a signed byte stream, as that's the basis of the thrift binary transport
;;; 


;;;
;;; abstract

(defclass vector-stream ()
  ((position
    :initform 0
    :reader get-stream-position :writer setf-stream-position)
   (vector
    :reader get-vector-stream-vector :writer setf-vector-stream-vector
    :type vector)
   (force-output-hook
    :initform nil :initarg :force-output-hook
    :accessor stream-force-output-hook
    :documentation "A function of one argument, the stream, called as the
     base implementation of stream-force-output.")
   #+(or CMU sbcl lispworks) (direction :initarg :direction)
   )
  (:default-initargs
    #+CormanLisp :element-type #+CormanLisp 'character))

(defClass vector-input-stream (vector-stream
                               #+ALLEGRO excl::fundamental-binary-input-stream
                               #+LispWorks stream:fundamental-stream
                               #+(and MCL digitool) ccl::input-binary-stream
                               #+(and MCL openmcl) fundamental-binary-input-stream
                               #+CMU extensions:fundamental-binary-input-stream
                               #+sbcl sb-gray:fundamental-binary-input-stream
                               #+CormanLisp stream
                               )
  ()
  (:default-initargs :direction :input))

(defClass vector-output-stream (vector-stream
                                #+ALLEGRO excl::fundamental-binary-output-stream
                                #+LispWorks stream:fundamental-stream
                                #+(and MCL digitool) ccl::output-binary-stream
                                #+(and MCL openmcl) fundamental-binary-output-stream
                                #+CMU extensions:fundamental-binary-output-stream
                                #+sbcl sb-gray:fundamental-binary-output-stream
                                #+CormanLisp stream
                                )
  ()
  (:default-initargs :direction :output))

(defclass vector-stream-transport (vector-input-stream vector-output-stream binary-transport)
  ())


(defun make-vector-stream-buffer (length &optional (type thrift::*binary-transport-element-type*))
  (make-array length :element-type type :initial-element 0))

(defmethod shared-initialize
           ((instance vector-stream) (slots t) &key (vector nil vector-s))
  (with-slots (position) instance
    (setf position 0)
    (when vector-s
      (setf-vector-stream-vector
       (etypecase vector
         (string (setf vector (map 'vector #'char-code vector)))
         (cl:list (setf vector (map 'vector #'(lambda (datum)
                                                (etypecase datum
                                                  (fixnum datum)
                                                  (character (char-code datum))))
                                    vector)))
         (vector vector))
       instance))
    (call-next-method)
    (unless (slot-boundp instance 'vector)
      (setf-vector-stream-vector (make-vector-stream-buffer 128) instance))))

#+cmu
(let ((old-definition (fdefinition 'stream-element-type)))
  (unless (typep old-definition 'generic-function)
    (fmakunbound 'stream-element-type)
    (defgeneric stream-element-type (stream))
    (setf (documentation 'stream-element-type 'function)
          (documentation old-definition 'function))
    (defmethod stream-element-type (stream)
      (funcall old-definition stream))))

(defmethod stream-element-type ((stream vector-stream))
  thrift::*binary-transport-element-type*)

(defmethod stream-position ((stream vector-stream) &optional new)
  (with-slots (vector) stream
    (if new
      (setf-stream-position (min (length vector) new) stream)
      (get-stream-position stream))))

(defmethod stream-eofp ((stream vector-stream))
  (with-slots (position vector) stream
    (>= position (length vector))))

(defmethod stream-finish-output ((stream vector-stream))
  nil)

(defmethod print-object
           ((vs vector-stream) (stream t)
            &aux (*print-array* t) (*print-length* 32) (*print-base* 16))
  (print-unreadable-object (vs stream :type t)
    (princ (get-vector-stream-vector vs) stream)))

(defmethod stream-force-output ((stream vector-stream))
  (let ((hook (stream-force-output-hook stream)))
    (when hook (funcall hook stream))))

;;;
;;; input

(defmethod stream-read-byte ((stream vector-stream-transport))
  (with-slots (position vector) stream
    (when (< position (length vector))
      (let ((byte (aref vector position)))
        (incf position)
        (if (> byte 127)
          (- (logxor 255 (1- byte)))
          byte)))))


(defmethod stream-read-sequence ((stream vector-input-stream) (sequence vector)
                                  #+mcl &key #-mcl &optional (start 0) (end nil))
  (unless end (setf end (length sequence)))
  (assert (typep start '(integer 0)))
  (assert (>= end start))
  (with-slots (vector position) stream
    (let* ((new-position (min (+ position (- end start)) (length vector))))
      (when (> new-position position)
        (replace sequence vector
                 :start1 start :end1 end
                 :start2 position :end2 new-position)
        (setf position new-position))
      new-position)))


;;;
;;; output


(defmethod stream-write-byte ((stream vector-output-stream) (datum integer) &aux next)
  (with-slots (position vector) stream
    (unless (< (setf next (1+ position)) (length vector))
      (setf vector
            (adjust-array vector (+ next (floor (/ next 4)))
                          :element-type thrift::*binary-transport-element-type*)))
    (setf (aref vector position)
          (logand #xff datum))
    (setf position next)))


(defmethod stream-write-sequence ((stream vector-output-stream) (sequence vector)
                                  #+mcl &key #-mcl &optional (start 0) (end nil))
  (unless end (setf end (length sequence)))
  (assert (typep start '(integer 0)))
  (assert (>= end start))
  (with-slots (vector position) stream
    (let* ((new-position (+ position (- end start))))
      (when (> new-position position)
        (unless (< new-position (length vector))
          (setf vector
                (adjust-array vector (floor (+ new-position (floor (/ new-position 4))))
                              :element-type thrift::*binary-transport-element-type*)))
        (replace vector sequence
                 :start1 position :end1 new-position
                 :start2 start :end2 end)
        (setf position new-position))
      new-position)))


;;;
;;;

#+(or)
(progn
  (stream-write-byte (make-instance 'vector-stream-transport) 1)
  (stream-write-byte (make-instance 'vector-stream-transport) -1)
  (let* ((data #(0 1 2 3 4 5 6 7 8 9 246 247 248 249 250 251 252 253 254 255))
         (buffer (make-array 2 :element-type thrift::*binary-transport-element-type*))
         (outstream (make-instance 'vector-output-stream :vector buffer))
         (instream (make-instance 'vector-input-stream :vector nil)))
    (write-sequence data outstream)
    (map nil #'(lambda (c) (stream-write-byte outstream (char-code c))) "asdf")

    (and (every #'eql
                (concatenate 'vector data (map 'vector #'char-code "asdf"))
                (subseq (get-vector-stream-vector outstream) 0 (stream-position outstream)))
         (let ((data2 (make-array (length data)))
               (data3 (make-array 4)))
           (setf-vector-stream-vector (get-vector-stream-vector outstream) instream)
           (and (eql (stream-read-sequence instream data2) (length data2))
                (equalp data2 data))
           (eql (stream-read-sequence instream data3) 4)
           (equal (map 'string #'code-char data3) "asdf"))))
  )


