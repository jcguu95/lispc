(in-package :paren)

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
