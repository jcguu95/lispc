(in-package :paren)
(setf (readtable-case *readtable*) :invert)

;;;

;; Debug
(defun inspect! (&rest rest)
  (break)
  rest)

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
  (cond ((symbolp form)
         (format nil "~a" (resolve-symbol form)))
        ((numberp form)
         (format nil "~a" form))
        ((stringp form)
         form)
        (t
         (let* ((operator (car form))
                (op-name (op-name operator)))
           (if (eq #\@ (char op-name 0))
               ;; funcall
               (let ((function-name (subseq op-name 1)))
                 (format nil "~a(~{~a~^, ~})"
                         (invert-case function-name)
                         (mapcar #'c (cdr form))))
               ;; operator call
               (let ((function (gethash op-name *functions*)))
                 (funcall function (cdr form))))))))

(defun resolve-symbol (symbol)
  (invert-case (substitute #\_ #\- (symbol-name symbol))))

(defun resolve-declaration (declaration)
  (assert (= 2 (length declaration)))
  (let* ((variable (resolve-symbol (nth 0 declaration)))
         (type (nth 1 declaration)))
    (format nil (fmt-string<-type type) variable)))

(defmacro def-cop (name vars &body body)
  "Define a C operation."
  (assert (= 1 (length vars)))
  `(setf (gethash (op-name ',name) *functions*)
         (lambda (,(car vars)) ,@body)))

(def-cop deftype (form)
  (let* ((type (nth 0 form))
         (kind-of-type (car type))
         (new-type (nth 1 form)))
    (case kind-of-type
      (:keyword
       (format nil "typedef ~a ~a;" (nth 1 type) new-type))
      (:struct
       (format nil "typedef struct ~a ~a;" (nth 1 type) new-type)))))

(def-cop defstruct (form)
  (let ((struct-name (nth 0 form))
        (cells (nth 1 form)))
    (format nil "struct ~a {~%~{  ~a;~^~%~}~%};"
            struct-name (mapcar #'resolve-declaration cells))))

(def-cop defun (form)
  (let ((func-name (nth 0 form))
        (arguments (nth 1 form))
        (body (cddr form)))
    (format nil "~a (~{~a~^, ~}) {~%~{  ~a;~%~}}"
            (resolve-declaration func-name)
            (mapcar #'resolve-declaration arguments)
            (mapcar #'c body))))

(def-cop ->  (form) (format nil "~{~a~^->~}" form))
(def-cop ==  (form) (format nil "(~a == ~a)" (c (nth 0 form)) (c (nth 1 form))))
(def-cop >   (form) (format nil "(~a > ~a)" (c (nth 0 form)) (c (nth 1 form))))
(def-cop <   (form) (format nil "(~a < ~a)" (c (nth 0 form)) (c (nth 1 form))))
(def-cop or  (form) (format nil "(~a || ~a)" (c (nth 0 form)) (c (nth 1 form))))
(def-cop and (form) (format nil "(~a && ~a)" (c (nth 0 form)) (c (nth 1 form))))
(def-cop return  (form) (format nil "return ~a" (car form)))

(def-cop str (form)
  (format nil "\"~a\"" (car form)))

(def-cop include (form)
  (concatenate
   'string
   (format nil "~{#include <~a>~%~}" (getf form :system))
   (format nil "~{#include \"~a\"~%~}" (getf form :local))))

(def-cop set (form)
  (let ((value (nth 1 form)))
    (if value
        (format nil "~a = ~a" (resolve-declaration (nth 0 form)) (c value))
        (format nil "~a"      (resolve-declaration (nth 0 form))))))

(def-cop vec (form)
  (format nil "{~{~a~^, ~}}" form))

