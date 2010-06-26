;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp; -*-

(asdf:defsystem :org.apache.thrift
  :nicknames (:thrift)
  :depends-on (:net.common-lisp.usocket
               :net.common-lisp.closer-mop
               :net.common-lisp.trivial-utf-8)
  :description "thrift implement common-lisp binding for the apache thrift communication protocol."
  :serial t
  :components ((:file "thrift"))

  :long-description
  "See : (among others)

   - [thrift lisp thread](http://markmail.org/thread/4tfa3zbweyg2qwne)
   - [whitepaper](http://incubator.apache.org/thrift/static/thrift-20070401.pdf) : The whitepaper from the facebook authors")
