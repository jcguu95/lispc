(in-package :paren)

(defparameter *functions* (make-hash-table :test #'equal))

(defun op-name (operator)
  (check-type operator symbol)
  (symbol-name operator))

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
           (if (and
                (> (length op-name) 1)
                (eq #\@ (char op-name 0)))
               ;; funcall
               (let ((function-name (subseq op-name 1)))
                 (format nil "~a(~{~a~^, ~})"
                         (invert-case function-name)
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

(defmacro def-cop (name vars &body body)
  "Define a C operator."
  (assert (= 1 (length vars)))
  `(setf (gethash (op-name ',name) *functions*)
         (lambda (,(car vars)) ,@body)))
