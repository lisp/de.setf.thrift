;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

(in-package :cl-user)

;;; sbcl
;;; (load "/development/source/library/build-init.lisp")
;;; (asdf:load-system :thrift)
;;; (let ((*compile-verbose* nil)) (compile-file "/development/source/library/de/setf/thrift/idl/cassandra-types.lisp"))
;;; (load "/development/source/library/de/setf/thrift/idl/cassandra-types")

;;; remote, for example
(defparameter *c-location*
  ;; remote
  ;; #u"thrift://ec2-174-129-66-148.compute-1.amazonaws.com:9160"
  ;; local
  #u"thrift://127.0.0.1:9160"
  )

(defparameter *c* (client *c-location*))


(cassandra:describe-keyspaces *c*)
;; => ("Keyspace1" "system")

(cassandra:describe-cluster-name *c*)
;; =>"Test Cluster"

(cassandra:describe-version *c*)
;; => "2.1.0"

(loop for space in (cassandra:describe-keyspaces *c*)
      collect (loop for key being each hash-key of (cassandra:describe-keyspace *c* space)
                    using (hash-value value)
                    collect (cons key
                                  (loop for key being each hash-key of value
                                        using (hash-value value)
                                        collect (cons key value)))))


(defun describe-cassandra (location &optional (stream *standard-output*))
  "Print the first-order store metadata for a cassandra LOCATION."

  (thrift:with-client (cassandra location)
    (let* ((keyspace-names (cassandra:describe-keyspaces cassandra))
           (cluster (cassandra:describe-cluster-name cassandra))
           (version (cassandra:describe-version cassandra))
           (keyspace-descriptions (loop for space in keyspace-names
                                        collect (cons space
                                                      (loop for key being each hash-key
                                                            of (cassandra:describe-keyspace cassandra space)
                                                            using (hash-value value)
                                                            collect (cons key
                                                                          (loop for key being each hash-key of value
                                                                                using (hash-value value)
                                                                                collect (cons key value))))))))
      (format stream "~&connection to : ~a" cassandra)
      (format stream "~&version : ~a" version)
      (format stream "~&cluster : ~a" cluster)
      (format stream "~&keyspaces~{~{~%~%space: ~a~@{~%  ~{~a :~@{~20t~:w~^~%~}~}~}~}~}" keyspace-descriptions))))

;;; (describe-cassandra *c-location*)

