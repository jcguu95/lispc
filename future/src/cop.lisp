(in-package :paren)

(def-cop lisp (form)
  ;; (log:debug `(progn ,@form))
  (eval `(progn ,@form)))

;; TODO Even `progn` is not a good name. Choose something better.
(def-cop progn-badname (form)
  (with-output-to-string (s)
    (loop :for subform :in form
          :for k :from 1
          :do (format s "~a;" (c subform))
          :do (when (< k (length form))
                (format s "~%")))))

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
    (format nil
            "~a (~{~a~^, ~}) {~%~a~%}"
            (resolve-declaration func-name)
            (mapcar #'resolve-declaration arguments)
            (indent (c (cons 'progn-badname body))))))

(def-cop ->  (form) (format nil "~{~a~^->~}"      (mapcar #'c form)))
(def-cop ==  (form) (format nil "(~a == ~a)"      (c (nth 0 form)) (c (nth 1 form))))
(def-cop >   (form) (format nil "(~a > ~a)"       (c (nth 0 form)) (c (nth 1 form))))
(def-cop <   (form) (format nil "(~a < ~a)"       (c (nth 0 form)) (c (nth 1 form))))
(def-cop +   (form) (format nil "(~{(~a)~^ + ~})" (mapcar #'c form)))
(def-cop -   (form) (format nil "(~{(~a)~^ - ~})" (mapcar #'c form)))
(def-cop *   (form) (format nil "(~{(~a)~^ * ~})" (mapcar #'c form)))
(def-cop /   (form) (format nil "(~{(~a)~^ / ~})" (mapcar #'c form))) ; TODO Add test.
(def-cop ++  (form) (format nil "(~a++)"          (c (nth 0 form))))
(def-cop --  (form) (format nil "(~a--)"          (c (nth 0 form))))
(def-cop or  (form) (format nil "(~a || ~a)"      (c (nth 0 form)) (c (nth 1 form))))
(def-cop and (form) (format nil "(~a && ~a)"      (c (nth 0 form)) (c (nth 1 form))))
(def-cop return (form) (format nil "return~a"     (if (nth 0 form)
                                                      (format nil " ~a" (c (nth 0 form)))
                                                      "")))

(def-cop str (form)
  (format nil "\"~a\"" (car form)))

(def-cop include (form)
  (concatenate
   'string
   (format nil "~{#include <~a>~^~%~}" (getf form :system))
   (format nil "~{#include \"~a\"~^~%~}" (getf form :local))))

(def-cop declare (form)
  (let ((value (nth 1 form)))
    (if value
        (format nil "~a = ~a"
                (resolve-declaration (nth 0 form))
                (c value))
        (format nil "~a"
                (resolve-declaration (nth 0 form))))))

(def-cop set (form)
  (assert (= (length form) 2))
  (format nil "~a = ~a"
          (c (nth 0 form))
          (c (nth 1 form))))

(def-cop vec (form)
  (format nil "{~{~a~^, ~}}" form))

(def-cop & (form)
  (format nil "&~a" (c (nth 0 form))))

(def-cop cond (form)
  (with-output-to-string (stream)
    (format stream "if ~a {~%~a~%}"
            (c (nth 0 (car form)))
            (indent (c (cons 'progn-badname (cdr (nth 0 form))))) ; NOTE progn-badname gives it semicolon..
            )
    (loop :for subform :in (butlast (cdr form))
          :do (format stream " else if ~a {~%~a~%}"
                      (c (nth 0 subform))
                      (indent (c (cons 'progn-badname (cdr subform))))))
    (let ((last-form (car (last (cdr form)))))
      (when last-form
        (if (eq t (nth 0 last-form))
            (format stream " else {~%~a~%}"
                    (indent (c (cons 'progn-badname (cdr last-form)))))
            (format stream " else if ~a {~%~a~%}"
                    (c (nth 0 last-form))
                    (indent (c (cons 'progn-badname (cdr last-form))))))))))

(def-cop case (form)
  (let ((result (format nil "switch (~a) {" (c (nth 0 form)))))
    (loop :for subform :in (cdr form)
          :do (if (eq t (car subform))
                  (setf result (format nil "~a~%  default:~%~a~%~a"
                                       result
                                       (indent (c (cons 'progn-badname (cdr subform))) :space-count 4)
                                       (indent "break;" :space-count 4)))
                  (setf result (format nil "~a~%  case ~a:~%~a~%~a"
                                       result
                                       (c (nth 0 subform))
                                       (indent (c (cons 'progn-badname (cdr subform))) :space-count 4)
                                       (indent "break;" :space-count 4)))))
    (setf result (format nil "~a~%}" result))))

(def-cop for (form)
  (with-output-to-string (stream)
    (format stream "for (~{~a~^; ~}) {~%" (mapcar #'c (nth 0 form)))
    (loop :for subform :in (cdr form)
          :do (format stream "  ~a~%" (c subform)))
    (format stream "}")))

(def-cop cast (form)
  (format nil "(~a)(~a)" (fmt-string<-type (nth 0 form)) (c (nth 1 form))))

(def-cop @ (form)
    (format nil "~a[~a]" (c (nth 0 form)) (c (nth 1 form))))

(def-cop goto (form)
  (format nil "goto ~a;" (c (nth 0 form))))

(def-cop label (form)
  (format nil "~a:" (c (nth 0 form))))

(def-cop block (form)
  (format nil "{~%~{~a~%~}~%}" (mapcar #'c form)))
