;;;  -*- Package: thrift-generated -*-

(in-package :thrift-generated)

;;; Embed the thrift AnnotationTest in the cl test suite

(thrift-test:test gen-cl/annotation-test
  (let ((struct (make-instance 'thrift-generated::foo
                  :bar -1 :baz -2 :qux -3)))
    (and (eql (thrift-generated::foo-bar struct) -1)
         (eql (thrift-generated::foo-baz struct) -2)
         (eql (thrift-generated::foo-qux struct) -3)
         (not (slot-boundp struct 'thrift-generated::bop)))))