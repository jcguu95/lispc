(in-package :paren)
(setf (readtable-case *readtable*) :invert)

;;;

(defparameter *functions* (make-hash-table :test #'equal))

;; A Utility Function
(defun invert-case (string)
  (if (string= "" string)
      string
      (loop :for index :from 0 :to (1- (length string))
            :do (cond
                  ((upper-case-p (char string index))
                   (setf (char string index) (char-downcase (char string index))))
                  ((lower-case-p (char string index))
                   (setf (char string index) (char-upcase   (char string index)))))
            :finally (return string))))

(defun op-name (operator)
  (check-type operator symbol)
  (symbol-name operator))

(defun c (form)
  "Compile lispy form into C code."
  (if (or (symbolp form)
          (numberp form))
      (invert-case (format nil "~a" form)) ; FIXME Case has to be flipped.
      (let* ((operator (car form))
             (op-name (op-name operator)))
        (if (eq #\@ (char op-name 0))
            (let ((function-name (subseq op-name 1)))
              (format nil "~a(~{~a~^, ~})"
                      (invert-case function-name)
                      (mapcar #'c (cdr form)))) ; FIXME Case of function-name has to be inverted.
            (let ((function (gethash op-name *functions*)))
              (funcall function (cdr form)))))))

(defun resolve-declaration (declaration)
  (let ((variable (nth 0 declaration)))
    (multiple-value-bind (type pointer-count)
        (resolve-type (nth 1 declaration))
      (format nil "~a ~a~a"
              type
              (make-string pointer-count :initial-element #\*)
              variable))))

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

(defmacro def-cop (name _ &body body)
  "Define a C operation."
  ;; FIXME FORM is a special keyword here. This is ugly. What should we do?
  (declare (ignore _))
  `(setf (gethash (op-name ',name) *functions*)
         (lambda (form) ,@body)))

(def-cop deftype ()
  (let ((type (nth 0 form))
        (new-type (nth 1 form)))
    ;; FIXME
    (format nil "typedef ~a ~a;" (resolve-type type) new-type)))

(def-cop defstruct ()
  (let ((struct-name (nth 0 form))
        (cells (nth 1 form)))
    (format nil "struct ~a {~%~{  ~a;~^~%~}~%};"
            struct-name (mapcar #'resolve-declaration cells))))

(def-cop defun ()
  (let ((func-name (nth 0 form))
        (arguments (nth 1 form))
        (body (cddr form)))
    (format nil "~a (~{~a~^, ~}) {~%~{  ~a;~%~}}"
            (resolve-declaration func-name)
            (mapcar #'resolve-declaration arguments)
            (mapcar #'c body))))

(def-cop ->  () (format nil "~{~a~^->~}" form))
(def-cop ==  () (format nil "(~a == ~a)" (c (nth 0 form)) (c (nth 1 form))))
(def-cop >   () (format nil "(~a > ~a)" (c (nth 0 form)) (c (nth 1 form))))
(def-cop <   () (format nil "(~a < ~a)" (c (nth 0 form)) (c (nth 1 form))))
(def-cop or  () (format nil "(~a || ~a)" (c (nth 0 form)) (c (nth 1 form))))
(def-cop and () (format nil "(~a && ~a)" (c (nth 0 form)) (c (nth 1 form))))
(def-cop str () (format nil "\"~a\"" (car form)))
(def-cop return  () (format nil "return ~a" (car form)))

(def-cop include ()
  (concatenate
   'string
   (format nil "~{#include <~a>~%~}" (getf form :system))
   (format nil "~{#include \"~a\"~%~}" (getf form :local))))

(def-cop set ()
  (let ((value (nth 1 form)))
    (if value
        (format nil "~a = ~a" (resolve-declaration (nth 0 form)) (c value))
        (format nil "~a"      (resolve-declaration (nth 0 form))))))
