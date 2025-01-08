(in-package :paren)

(defvar *functions* (make-hash-table :test #'equal))
(defvar *expression-operators* (make-hash-table :test #'equal))

(defun op-name (operator)
  (check-type operator symbol)
  (symbol-name operator))

(defun function-operator? (op-name)
  (and (> (length op-name) 1)
       (eq #\@ (char op-name 0))))

(defun c-expand-1 (form)
  ;; (log:debug form)
  (cond ((symbolp form)
         (format nil "~a" (resolve-symbol form)))
        ((numberp form)
         (format nil "~a" form))
        ((stringp form)
         form)
        ((listp form)
         (let* ((operator (car form))
                (op-name (op-name operator)))
           (if (function-operator? op-name)
               ;; funcall
               (let ((function-name (subseq op-name 1)))
                 (format nil "~a(~{~a~^, ~})"
                         (resolve-symbol (intern function-name))
                         (mapcar #'c (cdr form))))
               ;; operator call
               (let ((function (gethash op-name *functions*)))
                 (if function
                     (funcall function (cdr form))
                     (error "~a not found.~%" op-name))))))
        (t (error "Unrecognized type in form: ~a.~%" form))))

(defun c (form)
  "Compile lispy form into C code."
  (let ((expanded-form (c-expand-1 form)))
    (if (stringp expanded-form)
        expanded-form
        (c expanded-form))))

(defun set-expression (operator)
  "Recognize OPERATOR as a C operator denoting a C expression."
  ;; NOTE We need this because our compiler by default does not write
  ;; semicolons for expressions. But sometimes when it's standalone, an extra
  ;; semicolon has to be emit.
  ;;
  ;; NOTE We store strings just in case that the operators are parsed as
  ;; symbols in a difference package.
  (setf (gethash (string-downcase (op-name operator)) *expression-operators*) t))

(defun expression? (operator)
  (or (gethash (string-downcase (op-name operator)) *expression-operators*)
      (function-operator? (op-name operator))))

(defmacro def-cop (name vars &body body)
  "Define a C operator."
  (assert (= 1 (length vars)))
  `(progn
     ,(when (eq :expression (car body))
        (set-expression name))
     (setf (gethash (op-name ',name) *functions*)
           (lambda (,(car vars)) ,@body))))
