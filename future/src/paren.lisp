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

;; TODO Write a spec for types.
(defun resolve-type (type-spec)
  (cond
    ((stringp type-spec)
     type-spec)
    ((keywordp type-spec)
     (format nil "~a" type-spec))
    ((listp type-spec)
     (cond
       ((find (nth 0 type-spec) '(* :*))
        (if (integerp (nth 1 type-spec))
            ;; e.g. (* 2 :int) => (:pointer int 2)
            (list :pointer
                  (resolve-type (nth 2 type-spec))
                  (nth 1 type-spec))
            ;; e.g. (* :int) => (:pointer int 1)
            (list :pointer
                  (resolve-type (nth 1 type-spec))
                  1)))
       ((eq :struct (nth 0 type-spec))
        ;; e.g. (struct :int) => (:struct int)
        (list :struct (resolve-type (nth 1 type-spec))))
       ((eq :array (nth 0 type-spec))
        (list :array (nth 1 type-spec) (resolve-type (nth 2 type-spec))))))
    (t (error "Unsupported case."))))

(defun resolve-declaration (declaration)
  (let* ((variable (resolve-symbol (nth 0 declaration)))
         (type (resolve-type (nth 1 declaration))))
    (cond
      ((stringp type)
       (format nil "~a ~a" type variable))
      ((eq :pointer (car type))
       (let ((base-type (resolve-type (nth 1 type)))
             (pointer-count (nth 2 type)))
         (format nil "~a ~a~a"
                 ;; FIXME ugly code
                 (if (listp base-type)
                     (format nil "struct ~a" (nth 1 base-type))
                     base-type)
                 (make-string pointer-count :initial-element #\*)
                 variable)))
      ((eq :struct (car type))
       (let ((base-type (resolve-type (nth 1 type))))
         (format nil "struct ~a ~a" base-type variable)))
      ;; E.g.
      ;; (c '(set (x (:array 3 :int) (vec 1 2 3))))
      ;; (c '(set (x (:array nil :int) (vec 1 2 3))))
      ((eq :array (car type))
       (let ((length (nth 1 type))
             (base-type (resolve-type (nth 2 type))))
         (unless length (setf length ""))
         (format nil "~a ~a[~a]" base-type variable length)))
      (t (error "Unsupported case.")))))

(defmacro def-cop (name vars &body body)
  "Define a C operation."
  (assert (= 1 (length vars)))
  `(setf (gethash (op-name ',name) *functions*)
         (lambda (,(car vars)) ,@body)))

(def-cop deftype (form)
  (let* ((type (resolve-type (nth 0 form)))
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
