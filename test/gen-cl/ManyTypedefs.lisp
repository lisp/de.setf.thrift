;;;  -*- Package: thrift-generated -*-
;;;
;;; base operators definitions

(def-package :thrift-generated)

(defun method2 ()
  (list (map '(1 "string"))))

(defun method2 (int)
  (make-instance 'struct1 :myint int :mylist (list (map '(1 "string")))))

