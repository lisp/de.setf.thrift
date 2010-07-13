;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: org.apache.thrift.implementation; -*-

(in-package :org.apache.thrift.implementation)

;;; This file defines the thrift IDL operators for the `org.apache.thrift` library.
;;;
;;; copyright 2010 [james anderson](james.anderson@setf.de)
;;;
;;; Licensed to the Apache Software Foundation (ASF) under one
;;; or more contributor license agreements. See the NOTICE file
;;; distributed with this work for additional information
;;; regarding copyright ownership. The ASF licenses this file
;;; to you under the Apache License, Version 2.0 (the
;;; "License"); you may not use this file except in compliance
;;; with the License. You may obtain a copy of the License at
;;; 
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing,
;;; software distributed under the License is distributed on an
;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;; KIND, either express or implied. See the License for the
;;; specific language governing permissions and limitations
;;; under the License.


;;; The Common Lisp backend for the Thrift IDL translator[[1]] generates Lisp source code in terms of the
;;; following definition operators:
;;;
;;;   def-constant
;;;   def-eum
;;;   def-struct
;;;   def-exception
;;;   def-request-method
;;;   def-response-method
;;;   def-service
;;;
;;; The forms resemble those of the standard Lisp operators. The primary distinction is that identifiers are
;;; the original strings from the Thrift IDL source. The macro operators canonicalize and intern these
;;; according to the current package and read table case. The original values are retained to use as method
;;; and class names for encoding/decoding.
;;;
;;; The expansion process uses class definitions when expanding method codecs and IDL files frequently
;;; use structures for variable initialization. This suggests the following file organization and
;;; load order:
;;;
;;;   <service>-types.lisp : (generated) enums, structures, exceptions, services
;;;   <service>-vars.lisp : (generated) constants
;;;   <service.lisp : (written) the base function definitions
;;;
;;; [1]: $THRIFT/compiler/src/generate/t_cl_generator.cc


;;
;;; primitive constructors

(defun thrift:map (&rest tups)
  "Represent map objects as hash tables.
 NB. in order to effect equality when the keys themselves are maps, this and the transport operations
 would need to maintain a global registry."
  (declare (dynamic-extent tups))
  (let ((tbl (make-hash-table :test 'equal)))
     (loop for (key . value) in tups
           do (setf (gethash key tbl) value))
     tbl))

(defun thrift:list (&rest values)
  values)

(defun thrift:set (&rest values)
  values)


;;; control trace output from macros

(defvar *macroexpand-hooks* ())

(defun macroexpand-hook (macro whole result)
  (let ((hook (getf macro *macroexpand-hooks*)))
    (if hook
      (funcall hook whole result)
      result)))

(defmacro def-macro (name lambda-list &rest body)
  (let ((documentation (when (stringp (first body)) (pop body))))
    `(defmacro ,name (&whole whole ,@lambda-list)
       ,@(when documentation (list documentation))
       (let ((expansion (progn ,@body))
              (hook (getf *macroexpand-hooks* ',name)))
          (if hook
            (funcall hook whole expansion)
            expansion)))))

(defun trace-macro (whole expansion)
  (format *trace-output* "~&~%~:W~%=>~%~:W" whole expansion)
  expansion)

;;; (setf (getf *macroexpand-hooks* 'def-constant) 'trace-macro)

          
(defun parm-to-field-decl (parameter-spec)
  "Convert a specialize parameter declaration into the form for a structure field declaration
     (id-name type id)  -> (id-name default &key type id documentation)
 The format appearance in service method declarations is translated to the field for for use
 in request/response argument structures."

  (destructuring-bind (identifier type id &optional default) parameter-spec
    `(,identifier ,default :id ,id :type ,type)))


;;;
;;; definition operators

(def-macro def-package (name &key use)
  (let ((request-name (cons-symbol :keyword name :-request))
        (response-name (cons-symbol :keyword name :-response)))
    `(macrolet ((ensure-package (name &rest options)
                  `(let ((package (find-package ',name)))
                     (cond (package 
                            ,@(let ((use (assoc :use options)))
                                (when use `((use-package ',(rest use) package))))
                            package)
                           (t
                            (defpackage ,name ,@options))))))
       ;; the 'application' package is linked to cl and thrift with shodows
       (ensure-package ,name
                       (:use :common-lisp :thrift ,@use)
                       (:shadowing-import-from :common-lisp :byte :list :map :set :type-of))
       ;; the request/respone packages are isolated
       (ensure-package ,request-name (:use))
       (ensure-package ,response-name (:use)))))


(def-macro def-enum (identifier entries)
  (let ((name (cons-symbol *package* identifier)))
    ;; define the type, leave the keys are string
    (let ((values (mapcar #'rest entries)))
      (assert (stringp identifier))
      (assert (every #'integerp values))
      `(progn (setf (get ',name 'thrift::enum-members) ',values
                    (get ',name 'thrift::enum-alist) ',entries)
              ,@(mapcar #'(lambda (entry) `(defconstant ,(str-sym identifier "." (car entry)) ,(rest entry)))
                        entries)
              ',name))))


(def-macro def-constant (identifier val)
  "Generate a defparameter form, as the 'constants' are often bound to constructed values."
  (assert (stringp identifier))
  `(defparameter ,(str-sym identifier) ,val))


(def-macro def-struct (identifier fields &rest options)
  "DEF-STRUCT identifier [doc-string] ( field-specifier* ) option*
 [Macro]

 field-specifier ::= ( field-identifier default &key type id documentation )
 option ::= (:documentation docstring)
          | (:metaclass metaclass)
          | (:identifier identifier)

 Define a thrift struct with the declared fields. The class and field names are computed by cononicalizing the
 respective identifier and interning it in the current *package*. Each identifier remains associated with its
 metaobject for codec use. Options allow for an explicit identifier, a metacoal other than thrift-struct-class,
 and a documentation string.

 The class is bound to its name as bout the thrift class and CLOS class."

  (let ((metaclass (or (second (assoc :metaclass options)) 'thrift-struct-class))
        (identifier (or (second (assoc :identifier options)) identifier))
        (condition-class (second (assoc :condition-class options)))
        (name (str-sym identifier))
        (documentation nil))
    (when (stringp fields)
      (shiftf documentation fields (pop options)))
    ;; make the definitions available to compile codecs
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,name (thrift-object)
         ,(loop for field in fields
                collect (destructuring-bind (slot-identifier default &key type id documentation (optional nil o-s))
                                            field
                          (assert (typep id 'fixnum))
                          `(,(str-sym slot-identifier)
                            :initarg ,(cons-symbol :keyword slot-identifier)
                            :accessor ,(str-sym identifier "-" slot-identifier)
                            ,@(when type `(:type ,type))
                            :identifier-number ,id
                            :identifier-name ,identifier
                            ,@(when default `(:initform ,default))
                            ,@(when o-s `(:optional ,optional))
                            ,@(when documentation `(:documentation ,(string-trim *whitespace* documentation))))))
         (:metaclass ,metaclass)
         (:identifier ,identifier)
         ,@(when condition-class `((:condition-class ,condition-class)))
         ,@(when documentation `((:documentation ,(string-trim *whitespace* documentation)))))
       ,@(unless (eq metaclass 'thrift-exception-class)
           `((export ',name (symbol-package ',name))
             (setf (find-thrift-class ',name) (find-class ',name)))))))


(def-macro def-exception (identifier fields &rest options)
  "DEF-EXCEPTION identifier [doc-string] ( field-specifier* ) option*
 [Macro]

 field-specifier ::= ( field-identifier default &key type id documentation )
 option ::= (:documentation docstring)
          | (:metaclass metaclass)
          | (:identifier identifier)

 Define a thrift exception with the declared fields. This involves two classes. A condition is defined
 to use as a signal/error argument and a proxy struct class is defined for codec use.
 The proxy class is bound as the class name's thrift class, while the struct class is bound as the
 CLOS class."
  
  (let* ((metaclass (or (second (assoc :metaclass options)) 'thrift-exception-class))
         (identifier (or (second (assoc :identifier options)) identifier))
         (name (str-sym identifier))
         (struct-identifier (concatenate 'string identifier "ExceptionClass"))
         (struct-name (str-sym struct-identifier))
         (documentation nil))
    (when (stringp fields)
      (shiftf documentation fields (pop options)))
    ;; the definitions are used to compile codecs
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name (symbol-package ',name))
       (def-struct ,struct-identifier
         ,fields
         (:identifier ,identifier)
         (:metaclass ,metaclass)
         (:condition-class ,name)
         ,@options)
       (define-condition ,name (application-error)
         ,(loop for field in fields
                collect (destructuring-bind (slot-identifier default &key type id documentation optional)
                                            field
                          (declare (ignore id optional))
                          `(,(str-sym slot-identifier)
                            :initarg ,(cons-symbol :keyword slot-identifier)
                            :accessor ,(str-sym identifier "-" slot-identifier)
                            ,@(when type `(:type ,type))
                            ,@(when default `(:initform ,default))
                            ,@(when documentation `(:documentation ,(string-trim *whitespace* documentation))))))
         ,@(when documentation `((:documentation ,(string-trim *whitespace* documentation))))
         ,@(remove-if-not #'(lambda (key) (member key '(:default-initargs :documentation :report)))
                          options :key #'first))
       (defmethod thrift-error-format-control ((error ,name))
         (concatenate 'string (call-next-method)
                      ,(format nil "~{ ~a: ~~s~}." (mapcar #'first fields))))
       (defmethod thrift-error-format-arguments ((error ,name))
         (append (call-next-method)
                 (list ,@(loop for (slot-identifier) in fields
                               collect `(,(str-sym identifier "-" slot-identifier) error)))))
       (setf (find-thrift-class ',name) (find-class ',struct-name)))))



(defun generate-struct-decoder (prot class field-definitions extra-field-plist)
  "Generate a form which decodes a the given struct fiels in-line.
 PROT : a variable bound to a protocol instance
 FIELD-DEFINITIONS : a list of field definitions - either definition metaobjects or definition declarations
 EXTRA-FIELD-PLIST : a variable bound to a plist in which unknown fields are to be cached."

  (with-gensyms (value)
    `(loop (multiple-value-bind (name id read-field-type)
                                (stream-read-field-begin ,prot)
             (when (eq read-field-type 'stop) (return))
             (case id
               ,@(loop for fd in field-definitions
                       for id = (field-definition-identifier-number fd)
                       for field-type = (field-definition-type fd)
                       do (list fd id)
                       collect `(,id 
                                 (if (equal read-field-type ',(type-category field-type))
                                   (setf ,(field-definition-name fd)
                                         (stream-read-value-as ,prot ',field-type))
                                   (let ((,value (stream-read-value-as ,prot read-field-type)))
                                     (invalid-field-type ,prot ,class ,id name ',field-type ,value)
                                     ;; iff it returns
                                     (setf ,(field-definition-name fd) ,value)))))
               (t
                ;; handle unknown fields
                (let* ((value (stream-read-value-as ,prot read-field-type))
                       (fd (unknown-field ,class name id read-field-type value)))
                  (if fd
                    (setf (getf ,extra-field-plist (field-definition-initarg fd)) value)
                    (unknown-field ,prot name id read-field-type value)))))
             (stream-read-field-end ,prot)))))


(def-macro def-request-method (name (parameter-list return-type) &rest options)
  "Generate a request function definition.
 Augment the base function signature with an initial
 parameter for the connected protocol instance, Use that to manage the message construction,
 the request/reply process, and the result decoding. Return the result value or signal an
 exception as per the response."

  (let* ((identifier (or (second (assoc :identifier options)) (string name)))
         (exceptions (rest (assoc :exceptions options)))
         (exception-names (mapcar #'str-sym (mapcar #'car exceptions)))
         (oneway-p (second (assoc :oneway options)))
         (parameter-names (mapcar #'(lambda (a) (str-sym (first a))) parameter-list))
         (parameter-ids (mapcar #'third parameter-list))
         (type-names (mapcar #'(lambda (a) (type-name-class (second a))) parameter-list))
         (call-struct (or (second (assoc :call-struct options)) (str identifier "_args")))
         (reply-struct (or (second (assoc :reply-struct-type options)) (str identifier "_result")))
         (success (str-sym "success")))
    
    (with-gensyms (gprot extra-initargs)
      `(progn
         (ensure-generic-function ',name
                                  :lambda-list '(protocol ,@parameter-names)
                                  :generic-function-class 'thrift-request-function
                                  :identifier ,identifier)
         (defmethod ,name ((,gprot protocol) ,@(mapcar #'list parameter-names type-names))
           (stream-write-message-begin ,gprot ,identifier 'call
                                       (protocol-next-sequence-number ,gprot))
           ;; use the respective args structure as a template to generate the message
           (stream-write-struct ,gprot (thrift:map ,@(mapcar #'(lambda (id name) `(cons ,id ,name)) parameter-ids parameter-names))
                                ',(str-sym call-struct))
           (stream-write-message-end ,gprot)
           ,(if oneway-p
              nil
              `(multiple-value-bind (name type sequence)
                                    (stream-read-message-begin ,gprot)
                 (unless (eql sequence (protocol-sequence-number ,gprot))
                   (invalid-sequence-number ,gprot sequence (protocol-sequence-number ,gprot)))
                 (ecase type
                   (reply
                    (let (,@(unless (eq return-type 'void) `((,success nil)))
                          ,@(loop for name in exception-names collect `(,name nil))
                          (,extra-initargs nil))
                      ,(generate-struct-decoder gprot
                                                `(find-thrift-class ',(str-sym reply-struct))
                                                `(,@(unless (eq return-type 'void) `((,success nil :id 0 :type ,return-type)))
                                                  ,@exceptions)
                                                extra-initargs)
                      (stream-read-message-end ,gprot)
                      ,@(when exceptions
                          `((cond
                             ,@(mapcar #'(lambda (ex) `(,ex (response-exception ,gprot name sequence ,ex)))
                                       exception-names))))
                      ,(if (eq return-type 'void) nil success )))
                   ((call oneway)
                    ;; received a call/oneway when expecting a response
                    (unexpected-request ,gprot name sequence
                                        (prog1 (stream-read-struct ,gprot)
                                          (stream-read-message-end ,gprot))))
                   (exception
                    ;; received an exception as a response
                    (response-exception ,gprot name sequence
                                        (prog1 (stream-read-struct ,gprot)
                                          (stream-read-message-end ,gprot))))))))))))
    


(def-macro def-response-method (name (parameter-list return-type) &rest options)
  "Generate a response function definition.
 The method is defined with three arguments, a service, a sequence number and a protocol.
 The default method decodes the declared argument struct, invokes the base operator and, depending
 on the return type, encodes a response message. The given sequence number is reused in the response.
 The service argument is available for specialization, but otherwise ignored."

  (with-gensyms (service seq gprot extra-args)
    (let* ((identifier (or (second (assoc :identifier options)) (string name)))
           (oneway-p (second (assoc :oneway options)))
           (implementation (or (second (assoc :implementation-function options))
                               (error "An implementation function is required.")))
           (parameter-names (mapcar #'(lambda (a) (str-sym (first a))) parameter-list))
           (defaults (mapcar #'(lambda (a) (fourth a)) parameter-list))
           (call-struct (or (second (assoc :call-struct options)) (str identifier "_args")))
           (reply-struct (or (second (assoc :reply-struct options)) (str identifier "_result")))
           (exceptions (rest (assoc :exceptions options)))
           (application-form `(if ,extra-args
                                (apply #',implementation ,@parameter-names ,extra-args)
                                (,implementation ,@parameter-names))))
    
    `(progn (ensure-generic-function ',name
                                       :lambda-list '(service sequence-number protocol)
                                       :generic-function-class 'thrift-response-function
                                       :identifier ,identifier
                                       :implementation-function
                                       ,(etypecase implementation
                                          ;; defer the evaluation
                                          (symbol `(quote ,implementation))
                                          ((cons (eql lambda)) `(function ,implementation))))
              (defmethod ,name ((,service t) (,seq t) (,gprot protocol))
                (let (,@(mapcar #'list parameter-names defaults)
                      (,extra-args nil))
                  ,(generate-struct-decoder gprot `(find-thrift-class ',(str-sym call-struct))
                                            (mapcar #'parm-to-field-decl parameter-list) extra-args)
                  ,(let ((expression
                          (cond (oneway-p
                                 application-form)
                                ((eq return-type 'void)
                                 `(prog1
                                    ,application-form
                                    (stream-write-message-begin ,gprot ,identifier 'reply ,seq)
                                    (stream-write-struct ,gprot (thrift:map) ',(str-sym reply-struct))
                                    (stream-write-message-end ,gprot)))
                                (t
                                 `(let ((result ,application-form))
                                    (stream-write-message-begin ,gprot ,identifier 'reply ,seq)
                                    (stream-write-struct ,gprot (thrift:map (cons 0 result)) ',(str-sym reply-struct))
                                    (stream-write-message-end ,gprot)
                                    result)))))
                     (if exceptions
                       `(handler-case ,expression
                          ,@(loop for exception-spec in exceptions
                                  collect (destructuring-bind (field-name default &key type id)
                                                              exception-spec
                                            (declare (ignore field-name default))
                                            (let ((external-exception-type (second type)))
                                              `(,(str-sym external-exception-type) (condition)
                                                ;; sent as a reply in order to effect operation-specific exception
                                                ;; processing.
                                                (stream-write-message-begin ,gprot ,identifier 'reply ,seq)
                                                (stream-write-struct ,gprot (thrift:map (cons ,id condition))
                                                                     ',(str-sym reply-struct))
                                                (stream-write-message-end ,gprot)
                                                condition)))))
                       expression))))))))


(def-macro def-service (identifier base-services &rest options)
  "Given the external name for the service, an optional inheritance list, slot definitions
 and a list of method declarations, construct a class definition which include the precedence and the
 slots, and provides method bindings for the response methods as an initialization argument. For each method,
 generate a request/reponse method pair."
  
  (let* ((name (str-sym identifier))
         (class-identifier (second (assoc :class options)))
         (class (if class-identifier (str-sym class-identifier) 'service))
         (methods (remove :method options :test-not #'eq :key #'first))
         (documentation (second (assoc :documentation options)))
         (identifiers (mapcar #'second methods))
         (response-names (mapcar #'cons-response-symbol identifiers))
         (initargs (loop for (key . rest) in options
                         unless (member key '(:service-class :method :documentation))
                         collect key
                         and collect (list 'quote rest))))

    `(progn ,@(mapcan #'(lambda (method-declaration)
                          (destructuring-bind (identifier (parameter-list return-type) &key (oneway nil) (exceptions nil)
                                                          (implementation-function (str-sym  identifier)))
                                              (rest method-declaration)
                            (let* ((call-struct-identifier (str identifier "_args"))
                                   (reply-struct-identifier (str identifier "_result"))
                                   (request-function-name (cons-request-symbol identifier))
                                   (response-function-name (cons-response-symbol identifier)))
                              `((eval-when (:compile-toplevel :load-toplevel :execute)
                                  (def-struct ,call-struct-identifier
                                    ,(mapcar #'parm-to-field-decl parameter-list))
                                  (def-struct ,reply-struct-identifier
                                    (,@(unless (eq return-type 'void) `(("success" nil :id 0 :type ,return-type)))
                                     ,@exceptions)))
                                (export ',request-function-name (symbol-package ',request-function-name))
                                (export ',response-function-name (symbol-package ',response-function-name))
                                (def-request-method ,request-function-name (,parameter-list ,return-type)
                                  (:identifier ,identifier)
                                  (:call-struct ,call-struct-identifier)
                                  (:reply-struct ,reply-struct-identifier)
                                  ,@(when exceptions `((:exceptions ,@exceptions)))
                                  ,@(when oneway `((:oneway t))))
                                (def-response-method ,response-function-name (,parameter-list ,return-type)
                                  (:identifier ,identifier)
                                  (:call-struct ,call-struct-identifier)
                                  (:reply-struct ,reply-struct-identifier)
                                  (:implementation-function ,implementation-function)
                                  ,@(when exceptions `((:exceptions ,@exceptions)))
                                  ,@(when oneway `((:oneway t))))))))
                      methods)

            ;; export the service name only
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (export ',name (symbol-package ',name)))

            ;; construct and bind the global service instance
            (defparameter ,name
              (make-instance ',class
                :identifier ,identifier
                :base-services (list ,@(mapcar #'str-sym (if (listp base-services) base-services (list base-services))))
                :methods ',(mapcar #'(lambda (identifier name) `(,identifier . ,name))
                                   identifiers response-names)
                ,@(when documentation `(:documentation ,(string-trim *whitespace* documentation)))
                ,@initargs)))))


