(defpackage :paren.test
  (:use :common-lisp
        :paren
        :it.bese.fiveam))

(in-package :paren.test)



(setf (readtable-case *readtable*) :invert)

(defparameter *functions* (make-hash-table))

(defun c (form)
  "Compile lispy form into C code."
  (if (or (symbolp form)
          (numberp form))
      (format nil "~a" form)            ; FIXME Case has to be flipped.
      (let* ((operator (car form))
             (op-name (symbol-name operator)))
        (if (eq #\@ (char op-name 0))
            (let ((function-name (subseq op-name 1)))
              (format nil "~a(~{~a~^, ~})" function-name (mapcar #'c (cdr form)))) ; FIXME Case of function-name has to be inverted.
            (let ((function (gethash operator *functions*)))
              (funcall function (cdr form)))))))

(setf (gethash 'deftype *functions*)
      (lambda (form)
        (let ((type (nth 0 form))
              (new-type (nth 1 form)))
          (format nil "typedef ~a ~a;" type new-type))))

(test typedef
  (is (equal (c `(deftype (struct :x) x))
             "typedef struct X X;"))
  (is (equal (c `(deftype (struct :Y) Y))
             "typedef struct y y;")))

(setf (gethash 'defstruct *functions*)
      (lambda (form)
        (let ((struct-name (nth 0 form))
              (cells (nth 1 form)))
          (format nil "struct ~a {~%~{  ~a;~^~%~}~%};"
                  struct-name (mapcar #'resolve-declaration cells)))))

(defun resolve-declaration (declaration)
  (let ((variable (nth 0 declaration)))
    (multiple-value-bind (type pointer-count)
        (resolve-type (nth 1 declaration))
      (format nil "~a ~a~a"
              type
              (make-string pointer-count :initial-element #\*)
              variable))))

(resolve-declaration '(value :int))

(defun resolve-type (type-spec)
  (cond
    ((keywordp type-spec)
     (values (format nil "~a" type-spec) 0))
    ((listp type-spec)
     (assert (eq '* (car type-spec)))
     (if (integerp (nth 1 type-spec))
         (values (resolve-type (nth 2 type-spec))
                 (nth 1 type-spec))
         (values (resolve-type (nth 1 type-spec))
                 1)))
    (t (error "Unsupported case."))))

(test defstruct
  (is (equal
       (c `(defstruct X
             ((value :int)
              (next (* :X)))))
       "struct X {
  int value;
  X *next;
};")))

(test malloc-size-of
  (is (equal
       (c `(@malloc (@sizeof x)))
       "malloc(sizeof(X))")))

(setf (gethash '-> *functions*)
      (lambda (form)
        (format nil "~{~a~^->~}" form)))

(test ->
  (is (equal
       (c `(-> x1 value))
       "x1->value"))
  (is (equal
       (c `(-> x1 next next value))
       "x1->next->next->value"))
  (is (equal
       (c `(@printf (str "Answer: %d.\\n") (-> x1 next next value)))
       "printf(\"Answer: %d.\\n\", x1->next->next->value);")))

(setf (gethash 'str *functions*)
      (lambda (form)
        (format nil "~s" (car form))))

(test string
  (is (equal
       (c `(str "ABC"))
       "\"ABC\"")))

(setf (gethash '== *functions*)
      (lambda (form)
        (format nil "(~a == ~a)" (c (nth 0 form)) (c (nth 1 form)))))

(setf (gethash '> *functions*)
      (lambda (form)
        (format nil "(~a > ~a)" (c (nth 0 form)) (c (nth 1 form)))))

(setf (gethash '< *functions*)
      (lambda (form)
        (format nil "(~a < ~a)" (c (nth 0 form)) (c (nth 1 form)))))

(test ==
  (is (equal
       (c `(== i 0))
       "(i == 0)")))

(setf (gethash 'or *functions*)
      (lambda (form)
        (format nil "(~a || ~a)" (c (nth 0 form)) (c (nth 1 form)))))

(test or
  (is (equal
       (c `(or (== i 15)
               (== i 20)))
       "((i == 15) || (i == 20))")))

(setf (gethash 'and *functions*)
      (lambda (form)
        (format nil "(~a && ~a)" (c (nth 0 form)) (c (nth 1 form)))))

(test and
  (is (equal
       (c `(and (> i 0)
                (< i 30)))
       "((i > 0) && (i < 30))")))

(setf (gethash 'include *functions*)
      (lambda (form)
        (format nil "#include <~a>" (car form))))
;; TODO Implement include for local libs too.

(test include
  (is (equal
       (c `(include "stdio.h"))
       "#include <stdio.h>")))

(test defun
  (is (equal
       (c `(defun (main :int) ((argc :int) (argv :char**))
             (@printf (str "Hello, world!"))
             (return 0)))
       "int main (int argc, char **argv) {
  printf(\"Hello, world!\");
  return 0;
}"))
  (is (equal
       (c `(defun (main :int)
               (set (x :int) 5)
             (set (y :int) 10)
             (return 0)))
       "int main () {
  int x = 5;
  int y = 10;
  return 0 ;
}")))

(test assignment
  (is (equal
       (c `(set (x :int) 5))
       "int x = 5;"))
  (is (equal
       (c `(set (y :int) 10))
       "int y = 10;")))

(test printf
  (is (equal
       (c `(@printf (str "x = %d, y = %d \\n") x y))
       "printf(\"x = %d, y = %d\\n\", x, y);")))

(test return
  (is (equal
       (c `(return 0))
       "return 0;")))
