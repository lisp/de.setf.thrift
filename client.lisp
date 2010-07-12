;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: org.apache.thrift.implementation; -*-

(in-package :org.apache.thrift.implementation)

;;; This file defines client operators for the `org.apache.thrift` library.
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



(defgeneric client (location &key protocol direction element-type)
  (:method ((location puri:uri) &key (protocol 'binary-protocol) (direction :io)
            (element-type 'unsigned-byte))
    (make-instance protocol
      :transport (socket-transport location :element-type element-type :direction direction)
      :direction direction))

  (:method ((location pathname) &key (protocol 'binary-protocol) (direction :io)
            (element-type 'unsigned-byte))
    (let ((transport (make-instance 'file-transport
                        :element-type element-type
                       :direction direction
                       :pathname location)))
      (make-instance protocol :transport transport :direction direction))))


(defmacro with-client ((protocol &rest args) &body body)
  (with-gensyms (op)
    `(flet ((,op (,protocol) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-client #',op ,@args))))


(defun call-with-client (op &rest args)
  (declare (dynamic-extent args))
  (let ((protocol (apply #'client args)))
    (unwind-protect (funcall op protocol)
      (when (open-stream-p protocol)
        (close protocol)))))