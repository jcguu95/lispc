(in-package :paren)

(def-cop lisp (form)
  (eval `(progn ,@form)))

(def-cop compile-each (form)
  (let ((delimiter (car form))          ; default should be ""
        (form (cdr form)))
    (with-output-to-string (s)
      (loop :for subform :in form
            :for k :from 1
            :do (format s "~a" (c subform))
            :do (when (< k (length form))
                  (format s "~%~a" delimiter))))))

(def-cop deftype (form)
  (let* ((type (nth 0 form))
         (kind-of-type (car type))
         (new-type (nth 1 form)))
    (case kind-of-type
      (:keyword
       (format nil "typedef ~a ~a;"
               (c (nth 1 type)) (c new-type)))
      (:struct
       (format nil "typedef struct ~a ~a;"
               (c (nth 1 type)) (c new-type))))))

(def-cop defstruct (form)
  (let ((struct-name (nth 0 form))
        (cells (nth 1 form)))
    (format nil "struct ~a {~%~{  ~a;~^~%~}~%};"
            (c struct-name)
            (mapcar #'resolve-declaration cells))))

(def-cop defun (form)
  (let ((func-name (nth 0 form))
        (arguments (nth 1 form))
        (body (cddr form)))
    (format nil
            "~a (~{~a~^, ~}) {~%~a~%}"
            (resolve-declaration func-name)
            (mapcar #'resolve-declaration arguments)
            (indent (c `(compile-each "" ,body))))))

(def-cop enum (form)
  (format nil "enum { ~{~a~^, ~} }"
          (mapcar (lambda (x) (if (symbolp x) (resolve-symbol x) x))
                  (nth 1 form))))

(def-cop union (form)
  (format nil "~:
union ~a {~%~{  ~a;~%~}};"
          (nth 0 form)
          (mapcar #'resolve-declaration (cdr form))))

;; NOTE Do we need to use (~a) instead of ~a in ->?
(def-cop ->  (form) (format nil "~{~a~^->~}"      (mapcar #'c form)))
(def-cop ==  (form) (format nil "((~a) == (~a))"  (c (nth 0 form)) (c (nth 1 form))))
(def-cop <=  (form) (format nil "((~a) <= (~a))"  (c (nth 0 form)) (c (nth 1 form))))
(def-cop >=  (form) (format nil "((~a) >= (~a))"  (c (nth 0 form)) (c (nth 1 form))))
(def-cop !=  (form) (format nil "((~a) != (~a))"  (c (nth 0 form)) (c (nth 1 form))))
(def-cop >   (form) (format nil "((~a) > (~a))"   (c (nth 0 form)) (c (nth 1 form))))
(def-cop <   (form) (format nil "((~a) < (~a))"   (c (nth 0 form)) (c (nth 1 form))))
(def-cop +   (form) (format nil "(~{(~a)~^ + ~})" (mapcar #'c form)))
(def-cop -   (form) (format nil "(~{(~a)~^ - ~})" (mapcar #'c form)))
(def-cop *   (form) (format nil "(~{(~a)~^ * ~})" (mapcar #'c form)))
(def-cop /   (form) (format nil "(~{(~a)~^ / ~})" (mapcar #'c form))) ; TODO Add test.
(def-cop ++  (form) (format nil "((~a)++)"        (c (nth 0 form))))
(def-cop --  (form) (format nil "((~a)--)"        (c (nth 0 form))))
(def-cop or  (form) (format nil "((~a) || (~a))"  (c (nth 0 form)) (c (nth 1 form))))
(def-cop and (form) (format nil "((~a) && (~a))"  (c (nth 0 form)) (c (nth 1 form))))
(def-cop not (form) (format nil "(!(~a))"         (c (nth 0 form))))
(def-cop return (form) (format nil "return~a;"    (if (nth 0 form)
                                                      (format nil " (~a)" (c (nth 0 form)))
                                                      "")))

;; TODO defmacro may be enough?
(def-cop define (form)
  (assert (evenp (length form)))
  (with-output-to-string (s)
    (loop :for i :from 0 :to (1- (length form)) :by 2
          :do (format s "#define ~a ~a~%" (c (nth i form)) (c (nth (1+ i) form))))))

(def-cop defmacro (form)
  (prefix-newline-with-backslash
   (format nil "#define ~a(~{~a~^,~})~%~a"
           (c (nth 0 form))
           (mapcar #'resolve-symbol (nth 1 form))
           (c `(compile-each #\Newline ,(cddr form))))))

(def-cop undefmacro (form)
  (format nil "#undef ~a"
          (c (nth 0 form))))

(def-cop with-c-macro (form)
  (let ((macros (nth 0 form))
        (body (cdr form)))
    `(compile-each #\Newline
      ,@(loop :for macro :in macros
              :collect (cons 'defmacro macro))
      ,@body
      ,(loop :for macro :in macros
             :collect (cons 'undefmacro macro)))))

(def-cop include (form)
  (let ((system-libs (getf form :system))
        (local-libs (getf form :local)))
    (concatenate
     'string
     (format nil "~{#include <~a>~^~%~}" system-libs)
     (when (and system-libs local-libs)
       (format nil "~%"))
     (format nil "~{#include \"~a\"~^~%~}" local-libs))))

(def-cop while (form)
  (with-output-to-string (s)
    (format s "while (~a) {~%~a~%}"
            (c (nth 0 form))
            (indent (c `(compile-each "" ,(cdr form)))))))

(def-cop do-while (form)
  (format nil "do {~%~a~%} while (~a)"
          (indent (c `(compile-each "" ,(cdr form))))
          (c (nth 0 form))))

(def-cop str (form)
  (assert (= 1 (length form)))
  (format nil "\"~a\"" (car form)))

(def-cop char (form)
  (assert (= 1 (length form)))
  (format nil "'~a'" (car form)))

;; TODO Are there any other variations like this that I missed?
(def-cop wide-char (form)
  (assert (= 1 (length form)))
  ;; (car form) must be a character or a string of length 1.
  (format nil "L'~a'" (car form)))

(def-cop declare (form)
  (let ((value (nth 1 form)))
    (if value
        (format nil "~a = ~a;"
                (resolve-declaration (nth 0 form))
                (c value))
        (format nil "~a;"
                (resolve-declaration (nth 0 form))))))

(def-cop set (form)
  (assert (= (length form) 2))
  (format nil "~a = ~a;"
          (c (nth 0 form))
          (c (nth 1 form))))

(def-cop vec (form)
  (format nil "{~{~a~^, ~}}" form))

;; TODO May want to give a better name.. "at"?" "
(def-cop & (form)
  (format nil "&~a" (c (nth 0 form))))

;; TODO Define if, while, unless.
(def-cop cond (form)
  (with-output-to-string (stream)
    (format stream "if (~a) {~%~a~%}"
            (c (nth 0 (car form)))
            (indent (c `(compile-each "" ,(cdr (nth 0 form)))))
            )
    (loop :for subform :in (butlast (cdr form))
          :do (format stream " else if ~a {~%~a~%}"
                      (c (nth 0 subform))
                      (indent (c `(compile-each "" ,(cdr subform))))))
    (let ((last-form (car (last (cdr form)))))
      (when last-form
        (if (eq t (nth 0 last-form))
            (format stream " else {~%~a~%}"
                    (indent (c `(compile-each "" ,(cdr last-form)))))
            (format stream " else if ~a {~%~a~%}"
                    (c (nth 0 last-form))
                    (indent (c `(compile-each "" ,(cdr last-form))))))))))

(def-cop break (form)
  (assert (= 0 (length form)))
  "break;")

;; TODO Add test for exit.
;; TODO What's the difference with the cop "break"?
(def-cop exit (form)
  (format nil "break ~a;" (c (nth 0 form))))

(def-cop case (form)
  (let ((result (format nil "switch (~a) {" (c (nth 0 form)))))
    (loop :for subform :in (cdr form)
          :do (if (eq t (car subform))
                  (setf result (format nil "~a~%  default:~%~a~%~a"
                                       result
                                       (indent (c `(compile-each "" ,(cdr subform))) :space-count 4)
                                       (indent "break;" :space-count 4)))
                  (setf result (format nil "~a~%  case ~a:~%~a~%~a"
                                       result
                                       (c (nth 0 subform))
                                       (indent (c `(compile-each "" ,(cdr subform))) :space-count 4)
                                       (indent "break;" :space-count 4)))))
    (setf result (format nil "~a~%}" result))))

(def-cop for (form)
  (with-output-to-string (stream)
    (format stream "for (~{~a~^; ~}) {~%"
            (mapcar #'c (loop :for x :in (nth 0 form)
                              :collect (if (null x) "" x))))
    (loop :for subform :in (cdr form)
          :do (format stream "~a;~%" (indent (c subform))))
    (format stream "}")))

(def-cop cast (form)
  (format nil
          "(~a)(~a)"
          (fmt-string<-type (nth 0 form) t)
          (c (nth 1 form))))

(def-cop @ (form)
    (format nil "~a[~a]" (c (nth 0 form)) (c (nth 1 form))))

(def-cop goto (form)
  (format nil "goto ~a;" (c (nth 0 form))))

(def-cop label (form)
  (format nil "~a:" (c (nth 0 form))))

(def-cop block (form)
  (format nil "{~%~a~%}" (indent (c `(compile-each "" ,form)))))
