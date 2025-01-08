(in-package :cl-user)

(defpackage #:lispc
  (:use :common-lisp)
  (:export #:c
           #:c-file<-cl-file
           #:compile-and-run-cl-file))
