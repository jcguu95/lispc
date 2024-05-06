(in-package :paren)
(setf (readtable-case *readtable*) :invert)

;;;

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

(setf (gethash '-> *functions*)
      (lambda (form)
        (format nil "~{~a~^->~}" form)))

(setf (gethash 'str *functions*)
      (lambda (form)
        (format nil "~s" (car form))))

(setf (gethash '== *functions*)
      (lambda (form)
        (format nil "(~a == ~a)" (c (nth 0 form)) (c (nth 1 form)))))

(setf (gethash '> *functions*)
      (lambda (form)
        (format nil "(~a > ~a)" (c (nth 0 form)) (c (nth 1 form)))))

(setf (gethash '< *functions*)
      (lambda (form)
        (format nil "(~a < ~a)" (c (nth 0 form)) (c (nth 1 form)))))

(setf (gethash 'or *functions*)
      (lambda (form)
        (format nil "(~a || ~a)" (c (nth 0 form)) (c (nth 1 form)))))

(setf (gethash 'and *functions*)
      (lambda (form)
        (format nil "(~a && ~a)" (c (nth 0 form)) (c (nth 1 form)))))

;; TODO Implement include for local libs too.
(setf (gethash 'include *functions*)
      (lambda (form)
        (format nil "#include <~a>" (car form))))

(setf (gethash 'defun *functions*)
      (lambda (form)
        (let ((func-name (nth 0 form))
              (arguments (nth 1 form))
              (body (cddr form)))
          (format nil "~a (~{~a~^, ~}) {~%~{  ~a;~%~}}"
                  (resolve-declaration func-name)
                  (mapcar #'resolve-declaration arguments)
                  (mapcar #'c body)))))

(setf (gethash 'return *functions*)
      (lambda (form)
        (format nil "return ~a" (car form))))

(setf (gethash 'set *functions*)
      (lambda (form)
        (format nil "~a = ~a" (resolve-declaration (nth 0 form)) (c (nth 1 form)))))
