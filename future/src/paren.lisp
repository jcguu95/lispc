(in-package :paren)

;;;

(defparameter *functions* (make-hash-table :test #'equal))

;; Debug Util
(defun inspect! (&rest rest) (break) rest)

;; Utility
(defun invert-case (string)
  "If all English characters in the string are uppercase, convert them to lowercase.
If all are lowercase, convert them to uppercase. If the characters are mixed
case, leave the string unchanged."
  (if (string= "" string)
      string
      (let ((has-upper nil)
            (has-lower nil))
        ;; Classify the string
        (dolist (char (coerce string 'list))
          (cond
            ((upper-case-p char) (setf has-upper t))
            ((lower-case-p char) (setf has-lower t))))
        (cond
          ((and has-upper (not has-lower))
           (map 'string #'char-downcase string))
          ((and has-lower (not has-upper))
           (map 'string #'char-upcase string))
          (t
           string)))))

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

;;;

(defmacro def-cop (name vars &body body)
  "Define a C operator."
  (assert (= 1 (length vars)))
  `(setf (gethash (op-name ',name) *functions*)
         (lambda (,(car vars)) ,@body)))

(def-cop deftype (form)
  (let* ((type (nth 0 form))
         (kind-of-type (car type))
         (new-type (nth 1 form)))
    (case kind-of-type
      (:keyword
       (format nil "typedef ~a ~a;" (c (nth 1 type)) (c new-type)))
      (:struct
       (format nil "typedef struct ~a ~a;" (c (nth 1 type)) (c new-type))))))

(def-cop defstruct (form)
  (let ((struct-name (nth 0 form))
        (cells (nth 1 form)))
    (format nil "struct ~a {~%~{  ~a;~^~%~}~%};"
            (c struct-name) (mapcar #'resolve-declaration cells))))

(def-cop defun (form)
  (let ((func-name (nth 0 form))
        (arguments (nth 1 form))
        (body (cddr form)))
    (format nil "~a (~{~a~^, ~}) {~%~{  ~a;~%~}}"
            (resolve-declaration func-name)
            (mapcar #'resolve-declaration arguments)
            (mapcar #'c body))))

(def-cop ->  (form) (format nil "~{~a~^->~}" (mapcar #'c form)))
(def-cop ==  (form) (format nil "(~a == ~a)" (c (nth 0 form)) (c (nth 1 form))))
(def-cop >   (form) (format nil "(~a > ~a)" (c (nth 0 form)) (c (nth 1 form))))
(def-cop <   (form) (format nil "(~a < ~a)" (c (nth 0 form)) (c (nth 1 form))))
(def-cop or  (form) (format nil "(~a || ~a)" (c (nth 0 form)) (c (nth 1 form))))
(def-cop and (form) (format nil "(~a && ~a)" (c (nth 0 form)) (c (nth 1 form))))
(def-cop return (form) (format nil "return ~a" (c (nth 0 form))))

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

(def-cop & (form)
  (format nil "&~a" (c (nth 0 form))))
