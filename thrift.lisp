;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: thrift; -*-

(defpackage :thrift
  (:use :asdf :common-lisp)
  (:shadow :read-byte :write-byte :write-string)
  (:export serve simple-server handler
           tsocket topen tclose
           protocol binary-protocol binary-client
           thrift-error protocol-error transport-error))

(in-package :thrift)

(eval-when (compile load eval)
  (require 'asdf)
  (asdf:oos 'asdf:load-op 'usocket)
  (asdf:oos 'asdf:load-op 'trivial-utf-8))

(in-package :thrift)

(eval-when (compile load eval)
  (defun str (&rest args)
    (with-output-to-string (s)
      (dolist (x args)
        (princ x s))
      S)))

(defmacro mvb (bin form &body body)
  `(multiple-value-bind ,bin ,form ,@body))

(defmacro w/gensyms (syms &body b)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)
     ,@b))

(defmacro pincf (place)
  `(prog1 ,place (incf ,place)))

(defmacro dsb (arglist expr &body b)
  `(destructuring-bind ,arglist ,expr ,@b))

(define-condition thrift-error (error)
  ((message :initarg :message :initform "" :reader exception-message)))
(defmethod print-object :after ((x thrift-error) stream)
  (princ #\Space stream)
  (cl:write-string (slot-value x 'message) stream))

(defparameter *protocol-ex-unknown* 0)
(defparameter *protocol-ex-invalid-data* 1)
(defparameter *protocol-ex-negative-size* 2)
(defparameter *protocol-ex-size-limit* 3)
(defparameter *protocol-ex-bad-version* 4)

(define-condition protocol-error (thrift-error)
  ((type :initarg :type :reader protocol-error-type)))

(defun protocol-error (type &rest fmt)
  (error 'protocol-error :type type :message (apply #'format nil fmt)))

(define-condition transport-error (thrift-error) ())

(defun bytes-string (b)
  (trivial-utf-8:utf-8-bytes-to-string b))

(defun string-bytes (s)
  (trivial-utf-8:string-to-utf-8-bytes s))

(defclass ttransport () ())

(defgeneric topenp (transport))
(defgeneric topen (transport))
(defgeneric tclose (transport))
(defgeneric tread (transport))
(defgeneric twrite (transport string))
(defgeneric tflush (transport))

(defmethod treadall ((tr ttransport) len)
  (let ((a (make-array len :element-type '(unsigned-byte 8) :initial-element 0 :fill-pointer 0)))
    (dotimes (i len a)
      (vector-push (or (tread tr) (error 'transport-error :message "Remote side closed")) a))))

(defclass tsocket (ttransport)
  ((sock :initarg :sock :accessor tsocket-sock)
   (host :initarg :host :accessor tsocket-host)
   (port :initarg :port :accessor tsocket-port)))
(defun tsocket (host port)
  (make-instance 'tsocket :host host :port port))
(defmethod topenp ((s tsocket))
  ; usocket is crap, and can't actually test if sock has closed
  (and (not (null (tsocket-sock s)))
       (open-stream-p (usocket:socket-stream (tsocket-sock s)))))
(defmethod topen ((s tsocket))
  (setf (tsocket-sock s)
        (usocket:socket-connect (tsocket-host s) (tsocket-port s) :element-type '(unsigned-byte 8))))
(defmethod tclose ((s tsocket))
  (when (tsocket-sock s) (usocket:socket-close (tsocket-sock s))))
(defmethod tread ((s tsocket))
  (cl:read-byte (usocket:socket-stream (tsocket-sock s)) nil 'eof))
(defmethod twrite ((s tsocket) str)
  (funcall (if (arrayp str) #'write-sequence #'cl:write-byte)
           str
           (usocket:socket-stream (tsocket-sock s))))
(defmethod tflush ((s tsocket))
  ; should be provided by usocket; this won't always necessarily work
  (finish-output (usocket:socket-stream (tsocket-sock s))))
  
(defparameter *types*
  '((stop . 0)
    (void . 1)
    (bool . 2)
    (byte . 3)
    (double . 4)
    (i16 . 6)
    (enum . 8)
    (i32 . 8)
    (i64 . 10)
    (string . 11)
    (struct . 12)
    (map . 13)
    (set . 14)
    (list . 15)))

(defmacro ttype (name)
  `(cdr (assoc ',name *types*)))

(defparameter *message-types*
  '((call . 1)
    (reply . 2)
    (exception . 3)
    (unknown . -1)))

(defun message-type (type)
  (cdr (assoc type *message-types*)))

(defclass protocol ()
  ((trans :initarg :trans :accessor protocol-trans)
   (seq :initform 0 :accessor protocol-seq)))

(defmethod write-message-begin ((prot protocol) name type seq))
(defmethod write-message-end ((prot protocol)))
(defmethod write-struct-begin ((prot protocol) name))
(defmethod write-struct-end ((prot protocol)))
(defmethod write-field-begin ((prot protocol) name type id))
(defmethod write-field-end ((prot protocol)))
(defmethod write-field-stop ((prot protocol)))
(defmethod write-map-begin ((prot protocol) ktype vtype size))
(defmethod write-map-end ((prot protocol)))
(defmethod write-list-begin ((prot protocol) etype size))
(defmethod write-list-end ((prot protocol)))
(defmethod write-set-begin ((prot protocol) etype size))
(defmethod write-set-end ((prot protocol)))

(defparameter *base-read-methods* 
  '(message struct field map list set))

(defun define-base-read-method (type)
  `(progn 
     (defmethod ,(intern (str "READ-" type "-BEGIN")) ((prot protocol)))
     (defmethod ,(intern (str "READ-" type "-END")) ((prot protocol)))))

(dolist (m *base-read-methods*)
  (eval (define-base-read-method m)))

(defun primitivep (type)
  (find type '(bool byte i08 i16 i32 u64 i64 double string)))

(defclass binary-protocol (protocol) ())
(defparameter *binary-version-mask*  #xffff0000)
(defparameter *binary-version-1*  #x80010000)

(defmethod write-bool ((prot binary-protocol) val)
  (write-byte prot (if val 1 0)))

(defmethod write-byte ((prot binary-protocol) val)
  (twrite (protocol-trans prot) val))

(defun int-bytes (int size)
  (if (< int 0)
      (int-bytes (logxor (1- (expt 256 size)) (- 0 int 1))
                 size)
      (do ((a (make-array size :element-type '(unsigned-byte 8) :fill-pointer 0 :initial-element 0))
           (i 0))
          ((> (incf i) size) (reverse a))
        (mvb (x y) (truncate int 256)
          (setf int x)
          (vector-push y a)))))

(deftype signed-integer (bits)
  `(integer ,(- (expt 2 (1- bits))) ,(1- (expt 2 (1- bits)))))

(defmethod write-i16 ((prot binary-protocol) val)
  (declare (type (signed-integer 16) val))
  (twrite (protocol-trans prot) (int-bytes val 2)))

(defmethod write-i32 ((prot binary-protocol) val)
  (declare (type (signed-integer 32) val))
  (twrite (protocol-trans prot) (int-bytes val 4)))

(defmethod write-i64 ((prot binary-protocol) val)
  (declare (type (signed-integer 64) val))
  (twrite (protocol-trans prot) (int-bytes val 8)))

(defmethod write-double ((prot binary-protocol) val)
  #+allegro (dolist (b (mapcar #'(lambda (x) (int-bytes x 2))
                               (multiple-value-list (excl:double-float-to-shorts
                                                     (coerce val 'double-float)))))
              (twrite (protocol-trans prot) b))
  #-allegro (error "write-double isn't implemented on this platform"))

(defmethod write-string ((prot binary-protocol) val)
  (write-i32 prot (length val))
  (twrite (protocol-trans prot) (string-bytes val)))

(defmacro write-as (prot types &rest values)
  (assert (= (length types) (length values)))
  (let ((g (gensym)))
    `(let ((,g ,prot))
       ,@(mapcar #'(lambda (type val)
                     (list (intern (str "WRITE-" type)) g val))
                 types values))))

(defmethod write-message-begin ((prot binary-protocol) n ty s)
  (write-as prot (i32 string i32)
            (logior *binary-version-1* ty)
            n s))

(defmethod write-field-begin ((prot binary-protocol) n s i)
  (write-as prot (byte i16) s i))

(defmethod write-field-stop ((prot binary-protocol))
  (write-byte prot (ttype stop)))

(defmethod write-map-begin ((prot binary-protocol) k v s)
  (write-as prot (byte byte i32) k v s))

(defmethod write-list-begin ((prot binary-protocol) r s)
  (write-as prot (byte i32) r s))

(defmethod write-set-begin ((prot binary-protocol) ty s)
  (write-list-begin prot ty s))

(defmacro read-as (prot types)
  `(values ,@(mapcar #'(lambda (type)
                         (list (intern (str "READ-" type)) prot))
                     types)))

(defmethod read-byte ((prot binary-protocol))
  (tread (protocol-trans prot)))

(defun bytes-int (bytes &aux (m 0))
  (let ((val (reduce #'(lambda (tot n) (+ tot (* n (expt 256 (incf m))))) (reverse bytes))))
    (if (> (aref bytes 0) #x7f)
        (- 0 (logxor (- val 1) (1- (expt 256 (length bytes)))))
        val)))

(defmethod read-i16 ((prot binary-protocol))
  (bytes-int (treadall (protocol-trans prot) 2)))

(defmethod read-i32 ((prot binary-protocol))
  (bytes-int (treadall (protocol-trans prot) 4)))

(defmethod read-i64 ((prot binary-protocol))
  (bytes-int (treadall (protocol-trans prot) 8)))

(defmethod read-double ((prot binary-protocol))
  #+allegro (let ((b (treadall (protocol-trans prot) 8)))
              (apply #'excl:shorts-to-double-float
                     (mapcar #'bytes-int (list (subseq b 0 2) (subseq b 2 4)
                                               (subseq b 4 6) (subseq b 6 8)))))
  #-allegro (error "read-double isn't implemented on this platform"))
                  
(defmethod read-bool ((prot binary-protocol))
  (= (read-byte prot) 1))

(defmethod read-string ((prot binary-protocol))
  (let ((l (read-i32 prot)))
    (treadall (protocol-trans prot) l)))

(defmethod read-value ((prot binary-protocol) (type (eql 'bool)))
  (= (read-value prot 'byte) 1))

(defmethod read-message-begin ((prot binary-protocol))
  (declare (optimize (safety 3) (debug 3) (speed 0)))
  (let ((ver (read-i32 prot)))
    (if (/= (logand ver *binary-version-mask*) *binary-version-1*)
        (protocol-error *protocol-ex-bad-version*
                        "Wrong version identifier: ~A; expected ~A" ver *binary-version-1*)
        (let ((s (read-string prot))
              (sz (read-i32 prot)))
          (values s (logand ver #xff) sz)))))

(defmethod write-message-end ((prot binary-protocol))
  (tflush (protocol-trans prot)))

(defmethod read-field-begin ((prot binary-protocol))
  (let ((ty (read-byte prot)))
    (if (/= (ttype stop) ty)
        (let ((s (read-i16 prot)))
          (values "" ty s))
        (values "" ty 0)))) ; "" is the name, which isn't saved in binary prot

(defmethod read-map-begin ((prot binary-protocol))
  ; t_key t_val size
  (read-as prot (byte byte i32)))

(defmethod read-list-begin ((prot binary-protocol))
  ; t_elt size
  (read-as prot (byte i32)))

(defmethod read-set-begin ((prot binary-protocol))
  (read-list-begin prot))

;; generation

(defvar *gen-package* nil)

(defun gen-package ()
  (find-package *gen-package*))

(defun set-thrift-package (name)
  (setf *gen-package* name))

(defun str-sym (&rest strs)
  (let* ((s (apply #'str strs))
         (p (position #\: s))
         (pkg (gen-package)))
    (when p
      (setf pkg (find-package (intern (string-upcase (subseq s 0 p))))
            s (subseq s (1+ p))))
    (intern (string-upcase s) pkg)))

(defun strs-syms (strs &key (key #'identity))
  (mapcar #'str-sym (mapcar key strs)))

(defun sym-str (sym)
  ; we don't preserve case. does anything in thrift care?
  (string-downcase (string sym)))

(defun key-str (str)
  (intern (string-upcase str) 'keyword))

(defun gpkg (sym)
  (intern sym (gen-package)))

(defmacro def-enum (name vals)
  (let ((name (key-str name)))
    `(defmethod ,(gpkg 'enum) ((name (eql ,name)) val)
       (cdr (assoc val ',(mapcar #'(lambda (tup)
                                     (cons (key-str (car tup))
                                           (cdr tup)))
                                 vals))))))

(defmacro def-constant (name val)
  `(setf ,(str-sym name) ,val))

(defmacro def-hash-table (&rest tups)
  `(let ((tbl (make-hash-table :test 'equal)))
     (dolist (tup ',tups)
       (setf (gethash (car tup) tbl) (cdr tup)))
     tbl))

(defclass thrift-class (standard-class)
  ((types :initarg :types :accessor thrift-class-types)))

(defmacro def-struct (name parent vars)
  `(defclass ,(str-sym name)
       ,(cond ((null parent) nil)
              ((eql parent 'error) (list parent))
              (t (list (str-sym parent))))
     ,(mapcar #'(lambda (v)
                  (let ((fname (car v)))
                    `(,(str-sym fname) :initarg ,(key-str fname)
                       :accessor ,(str-sym name "-" fname))))
              vars)
     (:metaclass thrift-class)
     (:types ,@(mapcar #'(lambda (v)
                           (cons (str-sym (car v)) (cdr v)))
                       vars))))

(defun struct-types (struct)
  ; with unintentional support for inheritance of structs
  (let* ((cls (find-class struct))
         (par (car (c2mop:class-direct-superclasses cls)))) ; single
    (append (thrift-class-types cls)
            (if (or (eql par (find-class 'standard-object))
                    (eql par (find-class 'error)))
                '()
                (struct-types (class-name par))))))

(defun struct-slots (struct)
  (mapcar #'car (struct-types struct)))

(defclass client ()
  ((oprot :initarg :oprot :accessor client-oprot)
   (iprot :initarg :iprot :accessor client-iprot)))

(defmethod binary-client (client host port)
  (let ((s (tsocket host port)))
    (topen s)
    (let* ((b (make-instance 'binary-protocol :trans s)))
      (make-instance client :iprot b :oprot b))))

(defun service-name (str)
  (str-sym str "-client"))

(defun typespec-ttype (type)
  (cdr (assoc (if (consp type) (car type) type)
              *types*)))

(defun gen-prim-write-value (prot type val)
  `(,(intern (string-upcase (str "write-" type)))
     ,prot ,val))

(defun gen-prim-recv-value (prot type)
  `(,(intern (string-upcase (str "read-" type))) ,prot))

(defun gen-write-value (prot type val)
  (labels ((fail () (error "invalid type: ~A" type)))
    (cond ((primitivep type)
           (gen-prim-write-value prot type val))
          ((eql type 'void) nil)
          ((not (consp type)) (fail))
          ((eql (car type) 'struct)
           (gen-write-user-struct prot (cdr type) val))
          ((eql (car type) 'map)
           (gen-write-map prot (cdr type) val))
          ((eql (car type) 'list)
           (gen-write-list prot (cadr type) val))
          ((eql (car type) 'set)
           (gen-write-set prot (cadr type) val))
          ((eql (car type) 'enum)
           (gen-prim-write-value prot 'i32 val))
          (t (fail)))))

(defun gen-write-user-struct (prot type val)
  (let ((slots (struct-slots (str-sym (car type)))))
    (gen-write-struct prot (cadr type)
                           (mapcar #'(lambda (x) (cons (sym-str (car x)) (cdr x)))
                                   (struct-types (str-sym (car type))))
                           (mapcar #'(lambda (slot) `(slot-value ,val ',slot)) slots))))
         
(defun gen-write-struct (prot name params vals)
  ; params => ( ("foo" i32 1) )
  `(progn
     (write-struct-begin ,prot ,name)
     ,@(mapcar #'(lambda (param val)
                   `(progn
                      (write-field-begin ,prot ,(car param)
                                         ,(typespec-ttype (second param)) ,(third param))
                      ,(gen-write-value prot (cadr param) val)
                      (write-field-end ,prot)))
               params vals)
     (write-field-stop ,prot)
     (write-struct-end ,prot)))

(defun gen-recv-user-struct (prot type)
  (w/gensyms (s)
    (let ((struct (str-sym type)))
      `(let ((,s (make-instance ',struct)))
         ,(gen-recv-struct prot
                           (mapcar #'(lambda (ty)
                                       (cons `(slot-value ,s ',(first ty))
                                             (cdr ty)))
                                   (thrift-class-types (find-class struct))))
         ,s))))

(defun gen-recv-struct (prot params)
  ; params => ( ((slot-value foostruct bar) i32 1) )
  (w/gensyms (name type id loop break)
    `(progn
       (read-struct-begin ,prot)
       (tagbody ,loop
          (mvb (,name ,type ,id) (read-field-begin ,prot)
            (when (= ,type ,(ttype stop)) (go ,break))
            ,@(mapcar #'(lambda (p)
                          `(when (= ,id ,(third p))
                             (setf ,(first p) ,(gen-recv-value prot (second p)))
                             (read-field-end ,prot)
                             (go ,loop)))
                      params))
          ,break)
       (read-struct-end ,prot))))

(defun gen-recv-map (prot mapt)
  (w/gensyms (map k v size i key)
    `(let ((,map (make-hash-table :test 'equal))) ;should be eql?
       (mvb (,k ,v ,size) (read-map-begin ,prot)
         (dotimes (,i ,size)
           (let ((,key ,(gen-recv-value prot (car mapt))))
             (setf (gethash ,key ,map) ,(gen-recv-value prot (cadr mapt))))))
       (read-map-end ,prot)
       ,map)))

(defun gen-write-map (prot mapt tbl)
  (w/gensyms (k v)
    `(progn (write-map-begin ,prot
                             ,(typespec-ttype (car mapt))
                             ,(typespec-ttype (cadr mapt))
                             (hash-table-count ,tbl))
            (maphash #'(lambda (,k ,v)
                         ,(gen-write-value prot (car mapt) k)
                         ,(gen-write-value prot (cadr mapt) v))
                     ,tbl)
            (write-map-end ,prot))))

(defun gen-recv-seq (prot type flavour)
  (dsb (read-begin read-end)
      (if (eql flavour 'list)
          '(read-list-begin read-list-end)
          '(read-set-begin read-set-end))
    (w/gensyms (lst ty size i)
      `(let (,lst)
         (mvb (,ty ,size) (,read-begin ,prot)
           (dotimes (,i ,size)
             (push ,(gen-recv-value prot type) ,lst)))
         (,read-end ,prot)
         (nreverse ,lst)))))

(defun gen-recv-list (prot type)
  (gen-recv-seq prot type 'list))

(defun gen-recv-set (prot type)
  (gen-recv-seq prot type 'set))

(defun gen-write-seq (prot type flavour lst)
  (dsb (write-begin write-end)
      (if (eql flavour 'list)
          '(write-list-begin write-list-end)
          '(write-set-begin write-set-end))
    (w/gensyms (x)
      `(progn (,write-begin ,prot ,(typespec-ttype type) (length ,lst))
              (dolist (,x ,lst)
                ,(gen-write-value prot type x))
              (,write-end ,prot)))))

(defun gen-write-list (prot type lst)
  (gen-write-seq prot type 'list lst))

(defun gen-write-set (prot type set)
  (gen-write-seq prot type 'set set))

(defun gen-send-method (svc fn params tret async exceptions)
  (let ((names (strs-syms params :key #'car)))
    (w/gensyms (gsvc gprot)
      `(defmethod ,(str-sym fn) ((,gsvc ,svc) ,@names)
         (let ((,gprot (client-oprot ,gsvc)))
           (write-message-begin ,gprot ,fn ,(message-type 'call) (pincf (protocol-seq ,gprot)))
           ,(gen-write-struct gprot (str fn "_args")
                              params names)
           (write-message-end ,gprot)
           ,(unless async (gen-recv gsvc fn tret exceptions)))))))

(defun gen-recv-string (prot)
  `(bytes-string (read-string ,prot)))

(defun gen-recv-value (prot type)
  (labels ((fail () (error "invalid type: ~A" type)))
    (cond ((eql type 'string)
           (gen-recv-string prot))
          ((primitivep type)
           (gen-prim-recv-value prot type))
          ((eql type 'void) nil)
          ((not (consp type)) (fail))
          ((eql (car type) 'struct)
           (gen-recv-user-struct prot (cadr type)))
          ((eql (car type) 'map)
           (gen-recv-map prot (cdr type)))
          ((eql (car type) 'list)
           (gen-recv-list prot (cadr type)))
          ((eql (car type) 'set)
           (gen-recv-set prot (cadr type)))
          ((eql (car type) 'enum)
           `(read-i32 ,prot))
          (t (fail)))))

(defun gen-recv (gsvc fn ret exceptions)
  (let ((exs (mapcar #'str-sym (mapcar #'car exceptions))))
    (w/gensyms (gprot res)
      `(let ((,gprot (client-iprot ,gsvc))
             ,res ,@exs)
         (read-message-begin ,gprot)
         ,(gen-recv-struct gprot (append (list (list res ret 0))
                                         (mapcar #'(lambda (x ex)
                                                     (cons ex (cdr x))) exceptions exs)))
         (read-message-end ,gprot)
         ,@(mapcar #'(lambda (ex) `(when ,ex (error ,ex))) exs)
         ,res))))

(defun gen-fn-responder (fnspec)
  ; TODO what if one of the functions is from a different package to *gen-package*?
  (w/gensyms (iprot oprot hand res e)
    (labels ((respond (type params vals)
               `#'(lambda (,oprot)
                    (write-message-begin ,oprot ,(car fnspec) ,(message-type type)
                                         (pincf (protocol-seq ,oprot)))
                    ,(gen-write-struct oprot (str (car fnspec) "_result")
                                       params
                                       vals)
                   (write-message-end ,oprot))))
      (let ((args (mapcar #'(lambda (a) (str-sym (car a))) (fifth fnspec))))
        `#'(lambda (,iprot ,hand)
             (let ,args
               ,(gen-recv-struct iprot
                                 (mapcar #'(lambda (argspec)
                                             (cons (str-sym (car argspec))
                                                   (cdr argspec)))
                                         (fifth fnspec)))
               (handler-case (let ((,res (,(str-sym (car fnspec)) ,hand ,@args)))
                               ,(respond 'reply `(("success" ,(third fnspec) 0)) (list res)))
               ,@(mapcar #'(lambda (ex)
                             `(,(str-sym (cadadr ex)) (,e) ,(respond 'reply (list ex) (list e))))
                         (fourth fnspec)))))))))                                      

(defun gen-fntbl-entries (svc parent fns fntbl)
  `(setf ,@(reduce #'append
                   (mapcar #'(lambda (fnspec)
                               (list `(gethash ,(car fnspec) ,fntbl)
                                     (gen-fn-responder fnspec)))
                           fns))))

(defgeneric process (client handler input-protocol output-protocol &optional message))

(defun gen-processor (svc parent fns)
  (w/gensyms (hand name type seq iprot oprot fn fntbl rep msg)
    `(let ((,fntbl (make-hash-table :test 'string=))
           (,rep #'identity))
       ,(gen-fntbl-entries svc parent fns fntbl)
       (defmethod process :around ((client ,svc) ,hand ,iprot ,oprot &optional ,msg)
         (let (,name ,type ,seq)
           (if ,msg
               (dsb (name type seq) ,msg (setf ,name name ,type type ,seq seq))
               (mvb (name type seq) (read-message-begin ,iprot)
                 (setf ,name (bytes-string name) ,type type ,seq seq)))
           (when (= ,type ,(message-type 'call))
             (let ((,fn (gethash ,name ,fntbl)))
               (if ,fn 
                   (setf ,rep (funcall ,fn ,iprot ,hand))
                   (if (next-method-p)
                       (call-next-method client ,hand ,iprot ,oprot (list ,name ,type ,seq))
                       (error "~A: method not found" ,name)))))
           (read-message-end ,iprot)
           (funcall ,rep ,oprot))))))

(defmacro def-service (name parent &rest fns)
  (let ((svc (service-name name)))
    `(progn (defclass ,svc
                (,(if parent (service-name parent) 'client))
              ())
            ,@(mapcar #'(lambda (fnspec)
                          (dsb (fname async tret exceptions params) fnspec
                            (gen-send-method svc fname params tret async exceptions)))
                      fns)
            ,(gen-processor svc parent fns))))

(defclass handler () ())

(defclass server ()
  ((trans :accessor server-trans :initarg :trans)))
(defclass tcp-server (server) ())
(defclass simple-server (tcp-server) ())

(defmethod server-itrans ((s server) client)
  client)
(defmethod server-otrans ((s server) client)
  client)

(defmethod accept-connection ((s tcp-server))
  (make-instance 'tsocket :sock (usocket:socket-accept (server-trans s))))

(defmethod server-iprot ((s simple-server) trans)
  (make-instance 'binary-protocol :trans trans))
(defmethod server-oprot ((s simple-server) trans)
  (make-instance 'binary-protocol :trans trans))
(defmethod server-close ((s simple-server))
  (usocket:socket-close (server-trans s)))

(defun simple-server (host port)
  (make-instance 'simple-server
                 :trans (usocket:socket-listen host port :reuseaddress t)))

(defmethod serve ((s simple-server) client &optional (h (make-instance 'handler)))
  (unwind-protect (loop 
                     (let* ((cli (accept-connection s))
                            (itr (server-itrans s cli))
                            (otr (server-otrans s cli))
                            (iprot (server-iprot s itr))
                            (oprot (server-oprot s otr)))
                       (tagbody loop
                          (handler-case
                              (progn
                                (process client h iprot oprot)
                                (if (topenp cli)
                                    (go loop)
                                    (go close)))
                            (protocol-error (error)
                              (declare (ignore error))
                              (go close)))                          
                          close                          
                          (tclose itr)
                          (tclose otr))))
    (server-close s)))

