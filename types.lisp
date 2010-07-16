;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: org.apache.thrift.implementation; -*-

(in-package :org.apache.thrift.implementation)

;;; This file defines types for the `org.apache.thrift` library.
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

;;; Define type analogues between thrift and lisp types.
;;; The container types are defined to accept element type constraints.
;;; Distinguish those types which are lisp/thrift homologues.
;;; Define types for the type specifiers themselves for use at compile-time.


(deftype bool () 'boolean)
(deftype thrift:byte () '(signed-byte 8))
(deftype i08 () '(signed-byte 8))
(deftype i16 () '(signed-byte 16))
(deftype i32 () '(signed-byte 32))
(deftype i64 () '(signed-byte 64))
;; double is standard
;; string is standard
(deftype double () 'double-float)
(deftype binary () '(array i08 (*)))


(deftype thrift:list (&optional element-type)
  "The thrift:list container type is implemented as a cl:list. The element type
 serves for declaration, but not discrimination."
  (declare (ignore element-type))
 'list)

(deftype thrift:set (&optional element-type)
  "The thrift:set container type is implemented as a cl:list. The element type
 serves for declaration, but not discrimination."
  (declare (ignore element-type))
  'list)

(deftype thrift:map (&optional key-type value-type)
  "The thrift:map container type is implemented as a cl:hash-table The key and value types
 serve for declaration, but not discrimination."
  (declare (ignore key-type value-type))
  'hash-table)


(deftype base-type () '(member bool thrift:byte i08 i16 i32 i64 double string binary))

(defun base-type-p (type)
  (typep type 'base-type))

(deftype container-type () '(cons (member thrift:set thrift:list thrift:map)))

(defun container-type-p (type)
  (typep type 'container-type))

(deftype struct-type () '(cons (eql struct)))

(defun struct-type-p (type)
  (typep type 'struct-type))

(deftype enum-type () '(cons (eql enum)))

(defun enum-type-p (type)
  (typep type 'enum-type))

(deftype primitive-type () `(or base-type container-type enum-type))

(defun primitive-type-p (type)
  (typep type 'primitive-type))

(deftype thrift-type () '(or primitive-type struct-type))

(defun thrift-type-p (type)
  (typep type 'thrift-type))

(deftype enum (set-name)
  (etypecase set-name
    (symbol )
    (string (setf set-name (str-sym set-name))))
  `(member ,@(get set-name 'thrift::enum-members)))

(deftype struct (&optional identifier)
  "The exception class hierarchy is disjount for that of strucs as data."
  (if identifier
    (str-sym identifier)
    '(or thrift-object thrift-error)))

(deftype field-size () `(integer 0 ,array-dimension-limit))


;;;
;;; type-of equivalent which is specific to thrift types

(defgeneric thrift:type-of (object)
  (:documentation "Implements an equivalent to cl:type-of, but return the most specific thrift
 type instead of the cl type. This is used to determine the encoding for for dynamically generated
 messages.")

  (:method ((value null))
    'bool)
  (:method ((value (eql t)))
    'bool)
  (:method ((value integer))
    (etypecase value
     (i08 'thrift:byte)
     (i16 'i16)
     (i32 'i32)
     (i64 'i64)))
  (:method ((value float))
   'double)
  (:method ((value string))
    'string)
  (:method ((value vector))
    'binary)
  (:method ((value hash-table))
    'thrift:map)
  (:method ((value list))
    'thrift:list))

(defgeneric type-name-class (type-name)
  (:documentation "Return the lisp type equivalent for the given thrift type.
 The value is universal. it is used to construct generic function lambda lists.
 Signal an error If no equivalent exists.")

  (:method ((type-name symbol))
    (declare (special *types-classes*))
    (or (cdr (assoc type-name *types-classes* :test #'eql))
        (error "Invalid type name: ~s." type-name)))

  (:method ((type-name cons))
    (ecase (first type-name)
      (enum 'integer)
      (struct (str-sym (second type-name)))
      ((thrift:list thrift:set) 'list)
      (thrift:map 'hash-table))))


(defgeneric type-category (type)
  (:documentation "Return the type name to match decoded values.")

  (:method ((type symbol)) type)

  (:method ((type cons)) (first type)))

;;;
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


;;;
;;; primitive accessors
;;; --- in prepration to support association lists as maps

(defgeneric map-get (map key &optional default)
  (:documentation "Retrieve the map entry for a given key.
 Permit a-list and hash table as representation.")

  (:method ((map list) key &optional default)
    (let ((pair (assoc key map :test #'equalp)))
      (if pair
        (rest pair)
        default)))

  (:method ((map hash-table) key &optional default)
    (gethash key map default)))


(defgeneric map-set (map key value)
  (:method ((map list) key value)
    (let ((pair (assoc key map :test #'equalp)))
      (if pair
        (setf (rest pair) value)
        (setf map (acons key value map)))
      map))

  (:method ((map hash-table) key value)
    (setf (gethash key map) value)
    map))

(define-setf-method map-get (map key &environment env)
  (multiple-value-bind (temps vals stores
                        store-form access-form)
                       (get-setf-method map env)
    (let ((store (gensym))
          (stemp (first stores))
          (ktemp (gensym)))
      (values (cons ktemp temps) (cons key vals) (list store)
              `(let ((,stemp (map-set ,access-form ,ktemp ,store)))
                 ,store-form
                 ,store)
              `(map-get ,access-form ,ktemp)))))


(defgeneric map-map (function map)
  (:method (function (map list))
    (loop for (key . value) in map
          do (funcall function key value))
    nil)
  (:method (function (map hash-table))
    (maphash function map)))