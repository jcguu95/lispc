;;;; Utility functions that have nothing to deal with C.
(in-package :lispc)

(defun pairify (xs)
  (if (null xs)
      nil
      (cons (list (car xs) (cadr xs))
            (pairify (cddr xs)))))

(defun parenify (x)
  (format nil "(~a)" x))

(defun str<- (x)
  (format nil "~a" x))

(defun str<-lst (xs)
  (format nil "~{~a~}" xs))

(defun str<repeat-n< (x &optional (n 1))
  (format nil "~{~a~}"
          (loop for i from 1 to n collect x)))
(assert
 (and
  (equal (str<repeat-n< 0   5) "00000")
  (equal (str<repeat-n< #\a 5) "aaaaa")))

(defun chars<-str (x)
  (loop :for c :across x :collect c))

(defun timestamp ()
  "Return current timestamp as a string."
  (format nil "~{~a~^-~}" (reverse (multiple-value-list (get-decoded-time)))))

(defun elapsed-time (sec)
  (let* ((min (floor (/ (floor sec) 60)))
         (hr  (floor (/ min 60)))
         (day (floor (/ hr  24))))
    (setf sec (floor sec))
    (setf sec (mod sec 60))
    (setf min (mod min 60))
    (setf hr (mod hr 24))
    (format nil "~a~a~2,'0d:~2,'0d"
            (if (zerop day) "" (format nil "~a days, " day))
            (if (zerop hr) "" (format nil "~2,'0d:" hr))
            min sec)))

;; (defun write-out (fmt-string)
;;   "Append string to *FILE-OUT*."
;;   (check-type fmt-string string)
;;   (if *file-out*
;;       (with-open-file (stream *file-out* :direction :output
;;                                          :if-exists :append
;;                                          :if-does-not-exist :create)
;;         (format stream fmt-string))
;;       (error "*FILE-OUT* unbound.")))

(defun file-string (filename)
  "Return the file content as a string."
  (format nil "~{~a~^~%~}"
          (with-open-file (stream filename :direction :input :if-does-not-exist nil)
            (when stream
              (loop :for line := (read-line stream nil 'done)
                    :until (eq line 'done)
                    :collect line)))))

(defun count-lines-in-file (filename)
  (let ((n 0))
    (with-open-file (stream filename :direction :input :if-does-not-exist nil)
      (if stream
          (loop for line = (read-line stream nil 'done)
                until (eq line 'done)
                do (incf n))))
    n))

(defun temp-filename (&optional extension)
  (labels ((genfilename ()
             (str<-lst `(temp ,(random 1.0) ,extension))))
    (let ((filename (genfilename)))
      (loop while (probe-file filename) do
        (setf filename (genfilename)))
      filename)))

(defun flatten (xs)
  (if (atom xs) (list xs) (mapcan #'flatten xs)))
(assert
 (and
  (equal (flatten nil) '(()))
  (equal (flatten 'x) '(x))
  (equal (flatten '(x)) '(x))
  (equal (flatten '(x y)) '(x y))
  (equal (flatten '((x y))) '(x y))
  (equal (flatten '((x) (y))) '(x y))
  (equal (flatten '((x))) '(x))
  (equal (flatten '((x) (((y))) ((z w)))) '(x y z w))))

(defun pad-right (lst item len)
  (if (>= (length lst) len)
      lst
      (append (pad-right lst item (1- len)) (list item))))
(assert (equal (pad-right '(1 2) 0 5) '(1 2 0 0 0)))

(defun fold/list (x)
  "The same as (fold/list/n x 1)."
  (if (listp x) x (list x)))

(defun fold/list/n (x &optional (n 1))
  (check-type n (integer 0))
  (if (zerop n)
      x
      (if (eq 1 n)
          (fold/list x)
          (mapcar (lambda (y) (fold/list/n y (1- n)))
                  (fold/list x)))))
(assert
 (and
  (equal (fold/list/n nil 3)           ())
  (equal (fold/list/n `x 3)           `(((x))))
  (equal (fold/list/n `(x) 3)         `(((x))))
  (equal (fold/list/n `((x)) 3)       `(((x))))
  (equal (fold/list/n `((x y)) 3)     `(((x)   (y))))
  (equal (fold/list/n `((x) (y)) 3)   `(((x)) ((y))))
  (equal (fold/list/n `(((x) (y))) 3) `(((x)   (y))))))

;; (defun fold/list//n (x &optional (n 1))
;;   ;; Currently unused.
;;   (cond
;;     ((<= n 0) x)
;;     ((atom x)
;;      (list (fold/list//n x (1- n))))
;;     ((null (cdr x))
;;      (list (fold/list//n (car x) (- n 2))))
;;     (t
;;      (list (fold/list//n x (1- n))))))

(defun split-list-at (xs elem)
  (check-type xs list)
  (labels ((helper (xs elem curr res)
             (if (null xs) (cons (reverse curr) res)
                 (if (eq (car xs) elem)
                     (helper
                      (cdr xs) elem nil (cons (reverse curr) res))
                     (helper
                      (cdr xs) elem (cons (car xs) curr) res)))))
    (reverse (helper xs elem nil nil))))
(assert (equal (split-list-at '(1 1 1 0 1 1 0 1 1) 0) '((1 1 1) (1 1) (1 1))))

(defun split-str-at (str char)
  (check-type str string)
  (check-type char character)
  (remove-if #'(lambda (x) (eq (length x) 0))
             (mapcar #'str<-lst (split-list-at (chars<-str str) char))))
(assert (equal (split-str-at "aa_aa__a_" #\_) '("aa" "aa" "a")))

(defun replace-char (before after str)
  "Replace every BEFORE char with AFTER char in the string STR."
  (check-type str    string)
  (check-type before character)
  (check-type after  character)
  ;; e.g. (replace-char #\- #\_ "a_b-c-d_e")      ; => "a_b_c_d_e"
  (str<-lst (mapcar (lambda (x) (if (eq x before) after x))
                    (chars<-str str))))

(defun a-Z? (char)
  "Check if CHAR is among a-Z."
  (member (char-upcase char)
          (chars<-str "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(defun numeric-string? (x)
  (check-type x string)
  (ignore-errors (numberp (read-from-string x))))

;; (defun gensym-n (&optional (n 1))
;;   (loop :for i :from 1 :to n
;;         :collect (gensym)))

(defun concat-symbols (&rest syms)
  (read-from-string (str<-lst syms)))
(assert (eq 'abc (concat-symbols 'a 'b 'c)))

(defun symbol-append-c (nym)
  (nth-value 0 (concat-symbols nym '-c)))
(assert (eq (symbol-append-c 'xyz) 'xyz-c))

(defun trim-symbol (x n)
  (check-type x symbol)
  (check-type n (integer 0))
  (read-from-string (subseq (str<- x) n)))
(assert (eq (trim-symbol 'abcdefg 4) 'efg))

(defun macroexpand-n (x &optional (n 1))
  (if (zerop n)
      x
      (macroexpand-n (macroexpand-1 x) (1- n))))

;; (defun macnx (macro-form &optional (n 1))
;;   (if (zerop n)
;;       macro-form
;;       (if (listp macro-form)
;;           (if (atom (car macro-form))
;;               (if (equal (macroexpand-1 macro-form) macro-form)
;;                   (mapcar #'(lambda (x) (macnx x n)) macro-form)
;;                   (macnx (macroexpand-1 macro-form) (1- n)))
;;               (mapcar #'(lambda (x) (macnx x n)) macro-form))
;;           macro-form)))

(defun replacify (vars subs template)
  (labels ((helper (v s temp)
             (if (eq temp v) s
                 (if (atom temp) temp
                     (mapcar #'(lambda (x) (helper v s x)) temp)))))
    (if (null vars)
        template
        (replacify (cdr vars) (cdr subs)
                   (helper (car vars)
                           (car subs)
                           template)))))
(assert (equal '(1 2 3 2 1 1)
               (replacify '(x y z) '(1 2 3) '(x y z y x x)) ))

(defmacro replacify-lambda (vars template)
  (let ((varlist (loop for i from 1 to (length vars) collect (gensym))))
    `(lambda ,varlist (replacify ',vars (list ,@varlist) ',template))))

;; (defmacro swap (a b)
;;   (let ((c (gensym)))
;;     `(let ((,c ,a))
;;        (setf ,a ,b)
;;        (setf ,b ,c)
;;        (setf ,c ,a))))

(defun replace-fn (old-fn new-fn form)
  (labels ((helper (form replace?)
             (if (atom form)
                 (if (and replace?
                          (eq form old-fn))
                     new-fn
                     form)
                 (when form
                   (if (atom (car form))
                       (cond
                         ((eq (car form) 'function)
                          (cons 'function (helper (cdr form) t)))
                         ((eq (car form) 'quote)
                          (print (cadr form))
                          `(quote ,(cadr form)))
                         (t
                          (cons (helper (car form) t)
                                (mapcar (lambda (x) (helper x nil))
                                        (cdr form)))))
                       (cons (helper (car form) t)
                             (mapcar (lambda (x) (helper x nil))
                                     (cdr form))))))))
    (helper form t)))
(assert (equal '(1 (1) ((1)) (((1))) 0 0 (1 0 0))
               (replace-fn 0 1 '(0 (0) ((0)) (((0))) 0 0 (0 0 0)))))
