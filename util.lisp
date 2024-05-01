;;;; Utility functions that have nothing to deal with C.

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

(defun write-out (fmt-string)
  "Append string to *FILE-OUT*."
  (check-type fmt-string string)
  (if *file-out*
      (with-open-file (stream *file-out* :direction :output
                                         :if-exists :append
                                         :if-does-not-exist :create)
        (format stream fmt-string))
      (error "*FILE-OUT* unbound.")))

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
                do (incr n))))
    n))

(defun temp-filename (&optional extension)
  (labels ((genfilename ()
             (str<-lst `(temp ,(random 1.0) ,extension))))
    (let ((filename (genfilename)))
      (loop while (probe-file filename) do
        (setf filename (genfilename)))
      filename)))

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

(defun concat-symbols (&rest syms)
  (read-from-string (str<-lst syms)))
(assert (eq 'abc (concat-symbols 'a 'b 'c)))

(defun macroexpand-n (x &optional (n 1))
  (if (zerop n)
      x
      (macroexpand-n (macroexpand-1 x) (1- n))))

