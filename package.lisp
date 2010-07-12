;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

;;; This file defines the packages for the `org.apache.thrift` library.
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

(defpackage :org.apache.thrift
  (:nicknames :thrift)
  (:use)
  
  (:documentation "This is the home package for the symbols in the library's interface.
 It uses no packages, but imports 'string' from :cl. It does export some symbols
 particular to Thrift types and/or operators which conflict with standard Common Lisp symbols.
 These must be selectively shadowed as per application requirements in a using package.")

  (:import-from :common-lisp
               :string)

  (:export 
   :application-error
   :binary-protocol
   :binary-transport
   :bool
   :byte
   :call
   :class-not-found
   :class-not-found-error
   :client with-client
   :def-constant
   :def-enum
   :def-exception
   :def-package
   :def-service
   :def-struct
   :direct-field-definition
   :double
   :effective-field-definition
   :element-type-error
   :enum
   :enum-type-error
   :exception
   :field-size-error
   :field-type-error
   :i08
   :i16
   :i32
   :i64
   :invalid-element-type
   :invalid-enum
   :invalid-field-size
   :invalid-field-type
   :invalid-protocol-version
   :invalid-struct-type
   :list
   :map
   :protocol
   :protocol-error
   :protocol-error
   :protocol-input-transport
   :protocol-output-transport
   :protocol-version-error
   :reply
   :serve
   :serve simple-server handler
   :service
   :set
   :shared-service
   :stream-read-binary
   :stream-read-bool
   :stream-read-double
   :stream-read-field
   :stream-read-i08
   :stream-read-i16
   :stream-read-i32
   :stream-read-i64
   :stream-read-list
   :stream-read-map
   :stream-read-message
   :stream-read-message-type
   :stream-read-set
   :stream-read-string
   :stream-read-struct
   :stream-read-type
   :stream-write-binary
   :stream-write-bool
   :stream-write-double
   :stream-write-field
   :stream-write-i08
   :stream-write-i16
   :stream-write-i32
   :stream-write-i64
   :stream-write-list
   :stream-write-map
   :stream-write-message
   :stream-write-message-type
   :stream-write-set
   :stream-write-string
   :stream-write-struct
   :stream-write-type
   :string
   :struct
   :struct-type-error
   :thrift
   :thrift-class
   :thrift-error
   :thrift-error
   :transport
   :transport-error
   :transport-error
   :type-of
   :unknown-field
   :unknown-field-error
   :unknown-method
   :unknown-method-error
   :void
   ))


(defpackage :org.apache.thrift.implementation
  (:use :common-lisp :org.apache.thrift)
  
  (:documentation "The is the package for the thrift implementation. It exports nothing, uses the
 :common-lisp and :thrift package for access to the respective interfaces. Those names which conflict, eg.
 cl:list v/s thrift:list, are imported the :common-lisp package and referenced with an explicit prefix
 from the :thrift package.
  It also imports names as required per run-time for access to standard floating point constants and gray
 stream operators.")

  (:shadowing-import-from :common-lisp :byte :set :list :map :type-of)

  #+ccl
  (:import-from :ccl
                :stream-write-byte :stream-read-byte
                :stream-close :stream-direction
                :stream-position
                :stream-read-sequence :stream-write-sequence
                :stream-force-output :stream-finish-output)

  #+clozure
  (:import-from :ccl
                :double-float-positive-infinity
                :double-float-negative-infinity
                #+ccl-1.4 :double-float-nan)

  #+sbcl
  (:import-from :sb-ext
                :double-float-positive-infinity
                :double-float-negative-infinity
                :single-float-positive-infinity
                :single-float-negative-infinity)

  #+sbcl
  (:import-from :sb-gray
                :stream-write-byte :stream-read-byte
                :stream-read-sequence :stream-write-sequence
                :stream-force-output :stream-finish-output)
  )
