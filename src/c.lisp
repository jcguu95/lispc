;;;; Copyright Jonathan Baca, 2016
;;;; Copyright Jin-Cheng Guu, 2024

(in-package :lispc)

;; (defvar *lbrac* #\[)
;; (defvar *file-out*      nil)
;; (defvar *exec-out*      nil)
(defvar *last-compiled* nil)
(defvar *c-synonyms*    (make-hash-table))
(defvar *macro-list*    (make-hash-table))
(defvar *template-list* (make-hash-table))

(defmacro lisp/c-macro (nym llist &rest code)
  (let ((helper (concat-symbols nym '-background))
        (args (gensym)))
    `(progn
       (defun ,helper ,llist ,@(replace-fn nym helper code))
       (defun/c ,nym (&rest ,args) (c (apply #',helper ,args))))))

(defmacro macropairs (m &rest xs)
  `(progn ,@(mapcar #'(lambda (x) `(,m ,@x)) (pairify xs))))

(defmacro sethash (k v hash)
  `(setf (gethash ,k ,hash) ,v))

(defmacro inhash (k hash)
  `(nth-value 1 (gethash ,k ,hash)))

(defun csyn (k v)
  (sethash k v *c-synonyms*))

(defmacro cunsyn (k)
  `(remhash ,k *c-synonyms*))

;; (defun change-file (file &optional is-h)
;;   (setf *exec-out* (c-strify file))
;;   (setf *file-out* (format nil "~a.~a" *exec-out* (if is-h #\h #\c))))

;; (defun change-exec (nym)
;;   (setf *exec-out* (c-strify nym)))

;; (defun compile-c (file-out exec-out)
;;   (uiop:run-program (format nil "gcc ~a -o ~a" file-out exec-out)))

(defmacro decr (x)
  `(setf ,x (1- ,x)))

(defun c-strify (x &optional just-downcase?)
  (if (stringp x)
      x
      (progn
        (setf x (str<- x))
        (check-type x string)
        (cond (just-downcase?
               (string-downcase x))
              ((numeric-string? x)
               x)
              ((eq (char x 0) #\!)
               (replace-char #\- #\_ (compile-form (subseq x 1))))
              ((eq (char x 0) #\=)
               (camelcase-c (compile-form (subseq x 1))))
              ((eq (char x 0) #\-)
               (Lcamelcase-c (compile-form (subseq x 1))))
              (t
               (replace-char #\- #\_ (string-downcase x)))))))

;; NOTE 'nym' seems to stand for 'symbol' or 'name'.
(defun file-ext (nym ext)
  (format nil "~a.~a" (c-strify nym) (c-strify ext)))

(defmacro def (a b)
  `(setf ,a (if ,a ,a ,b)))

(defmacro deff (x f)
  `(setf ,x (,f ,x)))

(defmacro func-syn (func syn)
  `(progn
     (defun ,syn (&rest args)
       (apply #',func args))
     (compile ',syn)))

(defmacro cfunc-syn (func syn)
  `(func-syn ,(symbol-append-c func) ,(symbol-append-c syn)))

(defmacro func-syns (func syns &rest yns)
  (deff syns fold/list)
  (setf syns (append syns yns))
  `(progn ,@(mapcar #'(lambda (syn) `(func-syn ,func ,syn)) syns)))

(defmacro cfunc-syns (func syns &rest yns)
  (deff syns fold/list)
  (setf syns (append syns yns))
  `(progn ,@(mapcar #'(lambda (syn) `(cfunc-syn ,func ,syn)) syns)))

(defmacro un (x)
  `(setf ,x (not ,x)))

(defmacro incr (x)
  `(setf ,x (1+ ,x)))

(defmacro defun/c (f args &body body)
  `(progn (defun ,(symbol-append-c f) ,args ,@body)
          (compile ',(symbol-append-c f))))

(defmacro binop2 (oper &key nlp nrp nym)
  (def nym oper)
  (un nlp)
  (un nrp)
  (labels ((helper (a b)
             (if a
                 `(format nil "(~a)" (compile-form ,b))
                 `(compile-form ,b))))
    `(defun/c ,nym (x y)
       (format nil "~a~a~a" ,(helper nlp 'x) ',oper ,(helper nrp 'y)))))

                                        ; does a left reduce
(defmacro lredop (oper &key nym nparen)
  (def nym oper)
  (let ((lp (if nparen "" "("))
        (rp (if nparen "" ")")))
    `(defun/c ,nym (&rest xs)
       (if (null xs) nil
           (if (= 1 (length xs))
               (format nil "~a~a~a" ,lp (compile-form (car xs)) ,rp)
               (format nil "~a~a~a~a~a~a~a"
                       ,lp
                       ,lp (compile-form (car xs)) ,rp
                       ',oper
                       (apply (function ,(symbol-append-c nym)) (cdr xs)) ,rp))))))

(defmacro rredop (oper &key nym nparen)
  (def nym oper)
  (let ((lp (if nparen "" "("))
        (rp (if nparen "" ")")))
    `(defun/c ,nym (&rest xs)
       (if (null xs) nil
           (if (= 1 (length xs))
               (format nil "~a~a~a" ,lp (compile-form (car xs)) ,rp)
               (format nil "~a~a~a~a~a~a~a" ,lp
                       (apply (function ,(symbol-append-c nym)) (butlast xs))
                       ',oper ,lp (compile-form (car (last xs))) ,rp ,rp))))))

(defmacro binop (oper &key nlp nrp nym nyms l r nparen)
  ;;; (format t "OPER:~a NYM:~a NYMS:~a NPAREN:~a~%" oper nym nyms nparen)
  (if nyms
      `(progn ,@(mapcar #'(lambda (x)
                            `(binop ,oper :nlp ,(un nlp)
                                          :nrp ,(un nrp)
                                          :nym ,x
                                          :l l
                                          :r r
                                          :nparen ,nparen))
                        nyms))
      (if (or l r)
          (if l
              `(lredop ,oper :nym ,nym :nparen ,nparen)
              `(rredop ,oper :nym ,nym :nparen ,nparen))
          `(binop2 ,oper :nlp ,nlp :nrp ,nrp :nym ,nym))))

(defmacro pre (oper &key nym nparen)
  `(defun/c ,nym (x)
     (format nil "~a~a~a~a" ',oper
             ,(if nparen "" "(") (compile-form x)
             ,(if nparen "" ")") )))

(defmacro post (oper &key nym nparen)
  `(defun/c ,nym (x)
     (format nil "~a~a~a~a"
             ,(if nparen "" "(") (compile-form x)
             ,(if nparen "" ")") ',oper)))

(defmacro prepost (oper &key post nym nparen nyms)
  (setf nym (if nym nym oper))
  (if nyms
      `(progn ,@(mapcar #'(lambda (x) `(prepost ,oper :post ,post :nym ,x :nparen ,nparen)) nyms))
      (if post
          `(post ,oper :nym ,nym :nparen ,nparen)
          `(pre ,oper :nym ,nym :nparen ,nparen))))

(defmacro preposts (&rest opers)
  `(progn ,@(mapcar #'(lambda (oper) `(prepost ,@(fold/list oper))) opers)))

(defmacro binops (&rest opers)
  `(progn ,@(mapcar #'(lambda (oper) `(binop ,@(fold/list oper))) opers)))

(defun c (&rest xs)
  "Compile lispc codes XS.
   NOTE An important function."
  (format nil "~{~a~^~(;~%~%~)~}" (mapcar #'compile-form xs)))

(defun capitalize-c (str)
  (format nil "~a~a"
          (string-upcase (char (str<- str) 0))
          (string-downcase (subseq (str<- str) 1))))

(defun uncapitalize-c (str)
  (format nil "~a~a"
          (string-downcase (char (str<- str) 0))
          (subseq (str<- str) 1)))

(defun lowercase-c (&rest strs)
  (format nil "~{~a~}" (mapcar #'string-downcase (mapcar #'str<- strs))))

(defun uppercase-c (&rest strs)
  (format nil "~{~a~}" (mapcar #'string-upcase (mapcar #'str<- strs))))

(defun camelcase-c (&rest strs)
  (setf strs
        (flatten (mapcan #'(lambda (x) (split-str-at x #\-)) (mapcar #'str<- strs))))
  (setf strs
        (flatten (mapcan #'(lambda (x) (split-str-at x #\_)) (mapcar #'str<- strs))))
  (format nil "~{~a~}" (mapcar #'capitalize-c strs)))

(defun dashify-c (&rest strs)
  (format nil "~{~a~^-~}" (mapcar #'compile-form strs)))

(defun lcamelcase-c (&rest strs)
  (setf strs
        (flatten (mapcan #'(lambda (x) (split-str-at x #\-)) (mapcar #'str<- strs))))
  (format nil "~a~{~a~}" (string-downcase (car strs)) (mapcar #'capitalize-c (cdr strs))))

(defmacro with-optional-first-arg (args nym default-value possible-values &body body)
  (let ((other (gensym)))
    `(let ((,nym (if (member (car ,args) ',possible-values)
                     (car ,args)
                     ',other)))
       (if (eq ,nym ',other)
           (setf ,nym ,default-value)
           (setf ,args (cdr ,args)))
       ,@body)))

;; (defun bar (&rest xs)
;;   (with-optional-first-arg xs atmos 'cloudy (cloudy sunny rainy)
;;     (with-optional-first-arg xs deg 0 (0 1 2 3 4 5)
;;       (list atmos deg xs))))

;; NOTE e.g. (compile-form `(header stdio))             ; => "#include <stdio.h>"
;; NOTE e.g. (compile-form `((header stdio) (? 1 2 3))) ; => (format nil "#include <stdio.h>~%;~%(1)?2:(3)")
(defun compile-form (x)
  "Compile form."
  (cond
    ((null x) "")
    ((atom x)
     (if (inhash x *c-synonyms*)
         (compile-form (gethash x *c-synonyms*))
         (c-strify x)))
    ((atom (car x))
     (if (and (> (length (str<- (car x))) 1)
              (not (fboundp (symbol-append-c (car x)))))
         (case (char (str<- (car x)) 0)
           (#\@ (apply #'call-c       (compile-form (trim-symbol (car x) 1)) (cdr x)))
           (#\[ (apply #'nth-c        (compile-form (trim-symbol (car x) 2)) (cdr x)))
           (#\] (apply #'arr-c        (compile-form (trim-symbol (car x) 1)) (cdr x)))
           (#\& (apply #'addr-c       (compile-form (trim-symbol (car x) 1)) (cdr x)))
           (#\^ (apply #'cast-c       (compile-form (trim-symbol (car x) 1)) (cdr x)))
           (#\* (apply #'ptr-c        (compile-form (trim-symbol (car x) 1)) (cdr x)))
           (#\. (apply #'mem-c        (compile-form (trim-symbol (car x) 1)) (cdr x)))
           (#\> (apply #'slot-c       (compile-form (trim-symbol (car x) 1)) (cdr x)))
           (#\= (apply #'camelcase-c  (str<- (trim-symbol (car x) 1)) (mapcar #'str<- (cdr x))))
           (#\% (apply #'lcamelcase-c (str<- (trim-symbol (car x) 1)) (mapcar #'str<- (cdr x))))
           (#\- (apply #'lcamelcase-c (str<- (trim-symbol (car x) 1)) (mapcar #'str<- (cdr x))))
           (t   (apply (symbol-append-c (car x)) (cdr x))))
         (apply (symbol-append-c (car x)) (cdr x))))
    (t (format nil "~{~a~^~(;~%~)~}" (mapcar #'compile-form x)))))

(defmacro setf-cof (x)
  ;; (let ((x 0)) (setf-cof x) x)                ; => "0"
  ;; (let ((x '(header stdio))) (setf-cof x) x)                ; => "#include <stdio.h>"
  `(setf ,x (compile-form ,x)))

(defmacro setf-cofs (x)
  ;; (let ((x '(0 1 2 3))) (setf-cofs x))         ; => '("0" "1" "2" "3")
  ;; (let ((x 0)) (setf-cofs x))                  ; => '("0")
  ;; (let ((x '((header header)))) (setf-cofs x)) ; => '("#include <header.h>")
  `(setf ,x (mapcar #'compile-form (fold/list ,x))))

;;; DEFINE THE C LANGUAGE

;; NOTE 'nyms' seems to stands for synonyms, symbols, or names.
(binops (=   :l t :nyms (= set let <- ":="))
        (!=  :l t :nyms (!= neq diff different))
        (==  :r t :nyms (== eq same))
        (<   :r t :nyms (< lt))
        (>   :r t :nyms (> gt))
        (<=  :r t :nyms (<= leq le))
        (>=  :r t :nyms (>= geq ge))
        (&&  :r t :nyms (&& and et und y))
        (&   :r t :nyms (& bit-and band .and bit-et bet .et bit-und bund .und bit-y by .y ))
        (&=  :l t :nyms (&= &-eq bit-and-eq band-eq .and-eq bit-et-eq bet-eq .et-eq bit-und-eq bund-eq
                            .und-eq bit-y-eq by-eq .y-eq bit-and= band= .and= bit-et= bet=
                            .et= bit-und= bund= .und= bit-y= by= .y= ))
        ("||":r t :nyms (or uel oder o))
        ("|" :r t :nyms (bit-or .or bor bit-uel .uel buel bit-oder .oder boder bit-o .o bo))
        ("|=":l t :nyms (bit-or-eq .or-eq bor-eq bit-uel-eq .uel-eq buel-eq bit-oder-eq
                                   .oder-eq boder-eq bit-o-eq .o-eq bo-eq bit-or= .or=
                                   bor= bit-uel= .uel= buel= bit-oder= .oder= boder= bit-o= .o= bo=))
        (+   :r t :nyms (+ plus add sum))
        (+=  :l t :nyms (+= plus-eq add-eq sum-eq plus= add= sum=))
        (-   :r t :nyms (- minus subtract sub))
        (-=  :l t :nyms (-= minus-eq subtract-eq sub-eq minus= subtract= sub=))
        (*   :r t :nyms (* times product mul multiply))
        (*=  :l t :nyms (*= times-eq product-eq mul-eq multiply-eq times= product= mul= multiply=))
        (/   :r t :nyms (/ quotient ratio div divide))
        (/=  :l t :nyms (/= quotient-eq ratio-eq div-eq divide-eq quotient= ratio= div= divide=))
        (%   :r t :nyms (% modulo mod remainder))
        (%=  :l t :nyms (%-eq modulo-eq mod-eq remainder-eq %= modulo= mod= remainder=))
        (<<  :r t :nyms (<< l-shift shift-left shl))
        (" << " :l t :nparen t :nym <<+) ;; for C++
        (" >> " :l t :nparen t :nym >>+) ;; for C++
        (= :l t :nparen t :nym =!)
        (<<= :l t :nyms (<<= l-shift-eq shift-left-eq shl-eq l-shift= shift-left= shl=))
        (>>  :r t :nyms (>> r-shift shift-right shr))
        (>>= :l t :nyms (>>= r-shift-eq shift-right-eq shr-eq r-shift= shift-right= shr=))
        )

(preposts (++ :post nil :nyms (++  inc +inc incr pre++ +1 ++n))
          (++ :post t   :nyms (+++ pinc inc+ pincr post++ 1+ n++))
          (-- :post nil :nyms (--  dec -dec decr pre-- -1 --n))
          (-- :post t   :nyms (--- pdec dec- pdecr post-- 1- n--))
          (-  :post nil :nyms (neg))
          (!  :post nil :nyms (! not un a flip))
          (~  :post nil :nyms (~ bit-not bit-un bit-a bit-flip))
          (*  :post t   :nyms (ptrtyp arg*) :nparen t))

;;; C Stuff

(defun/c arr-decl (&rest xs)
  (format nil "{~{~a~^~(, ~)~}}" (mapcar #'compile-form xs)))

(defun/c struct-decl (&optional nym &rest xs)
  (setf-cof nym)
  (format nil "(~a){~{~a~^~(, ~)~}}" nym (mapcar #'compile-form xs)))

(defun/c sym/add (&rest xs)
  (setf-cofs xs)
  (str<-lst xs))

;; (slot-c "a" "b" "c") ; => "(a)->b->c"
(defun/c slot (a &rest bs)
  (setf-cof a)
  (setf-cofs bs)
  (format nil "(~a)~a~{~a~^~(->~)~}" a (if bs "->" "") bs))

(defun/c mem (a &rest bs)
  (setf-cof a)
  (setf-cofs bs)
  (format nil "(~a)~a~{~a~^.~}" a
          (if bs
              "."
              "")
          bs))

(defun/c typ* (x &optional (n 1))
  (setf-cof x)
  (format nil "~a~{~a~}" x
          (loop for i from 1 to n
                collect #\*)))

(defun/c const (&rest xs)
  (format nil "const ~a"
          (apply #'var-c
                 (if (= 1 (length xs))
                     (list (car xs) nil)
                     xs))))

(defun/c syn (a b)
  (csyn a b)
  "")

(defun/c unsyn (a)
  (cunsyn a)
  "")

(defun/c progn (&rest xs)
  ;; (progn-c "1" "2" 'c)                    ; => (format nil "  1;~%  2;~%  c;")
  (format nil "~{  ~a;~^~%~}" (mapcar #'compile-form xs)))

(defun/c ? (test ifyes ifno)
  ;; (?-c "1" "2" "3") ; => "(1)?2:(3)"
  (setf-cof test)
  (setf-cof ifyes)
  (setf-cof ifno)
  (format nil "(~a)?~a:(~a)" test ifyes ifno))

(defun/c if (test &optional ifyes ifno)
  ;; (if-c "1" "2" "3") ; => (format nil "if(1) {~%   2;~%}else{~%   3;~%}")
  (setf-cof test)
  (setf-cof ifyes)
  (format nil "if(~a) {~%   ~a;~%}~a" test ifyes
          (if ifno
              (format nil "else{~%   ~a;~%}" (compile-form ifno))
              "")))

(defun/c cond (&rest pairs)
  ;; (cond-c '("1" "2") '("3" "4") '("5" "6"))
  ;; ; =>
  ;; "if(1) {
  ;;   2;
  ;; }else if(3){
  ;;    4;
  ;; }else if(5){
  ;;    6;
  ;; }"
  (format nil "if(~a) {~{~%  ~a;~}~%}~{~a~}" (compile-form (caar pairs))
          (mapcar #'compile-form (cdar pairs))
          (mapcar
           #'(lambda (pair)
               (format nil "else if(~a){~{~%   ~a;~}~%}" (compile-form (car pair))
                       (mapcar #'compile-form (cdr pair))))
           (cdr pairs))))

(defun/c ifs (&rest pairs)
  (format nil "if(~a) {~{~%  ~a;~}~%}~{~a~}" (compile-form (caar pairs))
          (mapcar #'compile-form (cdar pairs))
          (mapcar
           #'(lambda (pair)
               (format nil "if(~a){~{~%   ~a;~}~%}" (compile-form (car pair))
                       (mapcar #'compile-form (cdr pair))))
           (cdr pairs))))

(defun/c main (&rest body)
  (format nil "int main(int argc,char **argv)~a" (block-c body)))

(defun/c for (a b c &rest lines)
  (setf-cof a)
  (setf-cof b)
  (setf-cof c)
  (format nil "for(~a;~a;~a)~a" a b c (block-c lines)))

(defun/c while (test &rest lines)
  (setf-cof test)
  (format nil "while(~a) ~a" test (block-c lines)))

(defun/c do-while (test &rest lines)
  (setf-cof test)
  (format nil "do~awhile(~a)" (block-c lines) test))

(defun/c switch (var &rest pairs)
  (setf-cof var)
  (labels ((helper (pairs)
             (format nil "~a:~%   ~a~%~a" (compile-form (caar pairs))
                     (block-c (cdar pairs) nil)
                     (if (cdr pairs)
                         (helper (cdr pairs))
                         ""))))
    (format nil "switch(~a){~a}" var (helper pairs))))

(defun/c addr (x &optional (n 1))
  (setf-cof x)
  (format nil "~a(~a)" (str<repeat-n< #\& n) x))

(defun/c ptr (x &optional (n 1))
  (format nil "~{~a~}(~a)"
          (loop for i from 1 to n
                collect #\*)
          (compile-form x)))

(defun/c pt (x &optional (n 1))
  (format nil "~{~a~}~a"
          (loop for i from 1 to n
                collect #\*)
          (compile-form x)))

(defun/c nth (x &optional (n 0) &rest ns)
  (format nil "(~a)[~a]~a" (compile-form x) (compile-form n)
          (if ns
              (format nil "~{[~a]~}" (mapcar #'compile-form ns))
              "")))

(defun/c arr (x &optional n &rest ns)
  (format nil "~a[~a]~a" (compile-form x) (compile-form n)
          (if ns
              (format nil "~{[~a]~}" (mapcar #'compile-form ns))
              "")))

(defun/c call (nym &rest args)
  (format nil "~a(~{~a~^,~})" (compile-form nym) (mapcar #'compile-form args)))

(defun/c cuda/call (nym ijk &rest args)
  (setf-cof nym)
  (setf-cofs ijk)
  (format nil "~a<<<~{~a~^,~}>>>(~{~a~^,~})" nym ijk (mapcar #'compile-form args)))

(defun/c str (&rest x)
  (setf-cofs x)
  (format nil "\"~{~a~^ ~}\"" x))

(defun/c char (x)
  (setf-cof x)
  (format nil "'~a'" x))

(defun/c cast (nym &optional (typ 'int) &rest typs)
  (if typs
      (apply #'cast-c (cast-c nym typ) typs)
      (format nil "((~a)(~a))" (compile-form typ) (compile-form nym))))

(defun/c var (x &optional type init &rest modifiers)
  (setf-cof x)
  (setf-cof type)
  (format nil "~a~a~{~a~^,~}~a"
          (if modifiers
              (format nil "~{~a ~}" (mapcar #'compile-form modifiers))
              "")
          (if type
              (format nil "~a " type)
              "")
          (fold/list x)
          (if init
              (format nil "=~a" (compile-form init))
              "")))

(defun/c vars (x &optional (inter #\,) (newline t))
  (setf x (mapcar #'(lambda (y) (apply #'var-c (fold/list y))) (fold/list/n x 1)))
  (format nil
          (format nil "~~{~~a~~^~(~a~a~)~~}" inter
                  (if newline
                      #\Newline
                      ""))
          x))


(defun/c varlist (args) (vars-c args #\;))

(defun/c struct (nym &optional vars) (setf-cof nym)
  (csyn '***curr-class*** nym)
  (if vars
      (format nil "struct ~a{~%  ~a;~%}" nym (vars-c vars #\;))
      (format nil "struct ~a" nym)))

(defun/c union (nym &optional vars)
  (setf-cof nym)
  (if vars
      (format nil "union ~a{~%  ~a;~%}" nym (vars-c vars #\;))
      (format nil "union ~a" nym)))

(defun/c block (&optional lines (bracket t))
  (let ((preq "")
        (unempty (and lines (not (equal '(nil) lines)))))
    (when (eq 'const (car lines))
      (setf preq " const ")
      (setf lines (cdr lines)))
    (when (and (listp (car lines))
               (eq '-> (caar lines)))
      (setf preq (format nil "~a -> ~a" preq (compile-form (cadar lines))))
      (setf lines (cdr lines)))
    (format nil "~a~a~a~{   ~a~(;~%~)~}~a" preq
            (if bracket #\{ "")
            (if unempty #\Newline "")
            (if unempty (mapcar #'compile-form (fold/list lines)) nil)
            (if bracket #\} ""))))

(defun/c func (nym &optional typ vars &rest body)
  (setf-cof nym)
  (setf-cof typ)
  (format nil "~a ~a(~a)~a" typ nym (vars-c vars #\, nil)
          (if body (block-c body) "")))

(defun/c inline (arg)
  (format nil "inline ~a" (compile-form arg)))

(defun/c cuda/global (&rest args)
  (format nil "__global__ ~a" (apply #'func-c args)))

(defun/c cuda/device (&rest args)
  (format nil "__device__ ~a" (apply #'func-c args)))

(defun/c funcarg (nym typ &optional varforms)
  (setf-cof nym)
  (setf-cof typ)
  (setf-cofs varforms)
  (format nil "~a(*~a)(~{~a~^,~})" typ nym varforms))

(defun/c return (&optional x &rest ys)
  (setf-cof x)
  (format nil "return ~a~a~{~^ ~a~}" x
          (if ys
              #\;
              "")
          (if ys
              (mapcar #'compile-form ys)
              nil)))

(defun/c typedef (x &optional y)
  (setf-cof x)
  (format nil "typedef ~a ~a;~%" x
          (if y
              (compile-form y)
              "")))

(defun/c enum (nym &rest mems)
  (setf-cof nym)
  (setf-cofs mems)
  (format nil "enum ~a{~{~a~^~(, ~)~}};~%" nym mems))

(defun/c h-file (nym)
  (setf-cof nym)
  (format nil "~a.h" nym))

(defun/c str/add (&rest xs)
  (format nil "~{~a~}" (compile-form xs)))

(defun/c include (filename &key local)
  ;; (include-c "abc" :local nil) ; => "#include <abc>"
  ;; (include-c "abc" :local t)   ; => "#include \"abc\""
  (setf-cof filename)
  (format nil "#include ~a~a~a~%"
          (if local #\" #\<)
          filename
          (if local #\" #\>)))

(defun/c import (filename)
  (setf filename
        (if (stringp filename)
            filename
            (format nil "~a.cl" (compile-form filename))))
  (c-code<-file filename)
  (format nil "/* ~a LOADED */" filename))

(defun/c macro (nym &rest xs)
  (setf-cof nym)
  (format nil "~a(~{~a~^,~})" nym (mapcar #'compile-form (fold/list xs))))

(defun/c unsigned (x)
  (setf-cof x)
  (format nil "unsigned ~a" x))

(defun/c define (a b)
  (setf-cof a)
  (setf-cof b)
  (format nil "#define ~a ~a~%" a b))

(defun/c ifdef (expr)
  (setf-cof expr)
  (format nil "#ifdef ~a~%" expr))

(defun/c ifndef (expr)
  (setf-cof expr)
  (format nil "#ifndef ~a~%" expr))

(defun/c |IF#| (expr)
  (setf-cof expr)
  (format nil "#if ~a~%" expr))

(defun/c |ELSE#| nil
  "#else~%")

(defun/c endif nil
  "#endif~%")

(defun/c pragma (&rest xs)
  (setf-cofs xs)
  (format nil "#pragma ~{~a~^ ~}" xs))

(defun/c paren (x)
  (setf-cof x)
  (format nil "(~a)" x))

(defun/c comment (&rest xs)
  (let* ((small (eq (car xs) 's))
         (s (format nil "/* ~{~a~^ ~} */~%"
                    (mapcar #'compile-form (if small (cdr xs) xs))))
         (v (if small
                ""
                (format nil "/**~a**/~%"
                        (str<repeat-n< #\* (- (length s) 7))))))
    (format nil "~%~a~a~a~%" v s v)))

(defun/c header (nym &key local)
  (include-c (h-file-c nym) :local local))

(defun/c headers (&rest xs)
  (format nil "~{~a~}"
          (mapcar #'(lambda (x) (apply #'header-c (fold/list x))) xs)))

(defun/c cpp (&rest xs)
  (setf-cofs xs)
  (format nil "#~{~a~^ ~}" xs))

(defun/c lisp (x)
  (let ((s (eval x)))
    (if (stringp s)
        s
        "")))

(defun/c lispmacro (f llist &rest body)
  (if (and (fboundp (symbol-append-c f)) (not (inhash f *macro-list*)))
      (format nil "/**ERROR: \"~a\" ALREADY DEFINED.**/" f)
      (progn
        (eval `(defun/c ,f ,llist ,@body))
        (sethash f t *macro-list*)
        (format nil "/**DEFINED: \"~a\" (lispmacro)**/" f))))

(defun/c lisp/c-macro (nym llist &rest body)
  (eval `(lisp/c-macro ,nym ,llist ,@body))
  (format nil "/**LISP/C MACRO \"~a\"**/" nym))

(defun/c lambda (llist template &rest args)
  (compile-form (eval `(apply (replacify-lambda ,llist ,template) ',args))))

(defun/c template (f vars template)
  (eval `(defun/c ,f (&rest args)
           (compile-form (apply (replacify-lambda ,vars ,template) (mapcar #'compile-form args)))))
  (sethash f t *template-list*)
  (format nil "/**DEFINED: \"~a\" (template)**/" f))

(defun/c templates (f vars template)
  (eval
   `(defun/c ,f (&rest argss)
      (apply #'progn-c
             (mapcar #'compile-form
                     (mapcar
                      #'(lambda (args)
                          (apply (replacify-lambda ,vars ,template)
                                 (mapcar #'compile-form (fold/list args))))
                      argss)))))
  "")

(defun/c cuda/dim3 (typ x y)
  (setf-cof typ)
  (setf-cof x)
  (setf-cof y)
  (format nil "dim3 ~a(~a,~a)" typ x y))

(defun/c cuda/dim/block (x y)
  (cuda/dim3-c 'dim/block x y))

(defun/c cuda/dim/grid (x y)
  (cuda/dim3-c 'dim/grid x y))

(defun/c cuda/shared (&rest xs)
  (format nil "__shared__ ~a" (apply #'var-c xs)))

(defun/c repeat (x &optional (n 1))
  (setf-cof x)
  (format nil "~{~a~^ ~}"
          (loop :for i :from 1 :to n
                :collect x)))

(defun/c funcall (func &rest args)
  (apply (symbol-append-c func) args))

(defun/c apply (func &rest args)
  (setf args (append (butlast args) (car (last args))))
  (apply (symbol-append-c func) args))

(defun/c mapcar (&rest argss)
  (with-optional-first-arg argss brackets? nil (t nil)
    (let ((func (car argss)))
      (setf argss (cdr argss))
      (block-c (apply #'mapcar (symbol-append-c func) argss) brackets?))))

(defun/c mapargs (&rest argss)
  (with-optional-first-arg argss brackets? nil (t nil)
    (let ((func (car argss)))
      (setf argss (cdr argss))
      (block-c (mapcar #'(lambda (args) (apply-c func args)) argss)
               brackets?))))

(defun/c car (&rest args)
  (car args))

(defun/c cdr (&rest args)
  (cdr args))

(defun/c cadr (&rest args)
  (cadr args))

(defun/c cdar (&rest args)
  (cdar args))

(defun/c cddr (&rest args)
  (cddr args))

(defun/c caar (&rest args)
  (caar args))

(defun/c binop (opr &rest xs)
  (setf-cofs xs)
  (format nil (format nil "(~~{(~~a)~~^~~(~a~~)~~})" opr) xs))

(defun/c funcall-if (test func &rest args)
  (if test
      (apply #'funcall-c func args)
      (str<-lst (mapcar #'compile-form args))))

(defun/c apply-if (test func args)
  (if test
      (apply #'funcall-c func args)
      (str<-lst (mapcar #'compile-form args))))

(defun/c test-eq (a b)
  (eq a b))

(defun/c test-not (a)
  (not a))

(defun/c test-and (&rest xs)
  (eval `(and ,@xs)))

(defun/c test-or (&rest xs)
  (eval `(or ,@xs)))

(defun/c code-list (&rest xs)
  (mapcar #'compile-form xs))

(defun/c list (&rest xs)
  xs)

;;; C++ Stuff
(defun/c hh-file (nym)
  (setf-cof nym)
  (format nil "~a.hh" nym))

(defun/c header++ (nym &key local)
  (if local
      (include-c (hh-file-c nym) :local local)
      (include-c nym)))

(defun/c headers++ (&rest xs)
  (format nil "~{~a~}"
          (mapcar #'(lambda (x) (apply #'header++-c (fold/list x))) xs)))

(defun/c tridot (x)
  (setf-cof x)
  (format nil "~a..." x))

(defun/c struct++ (&optional nym &rest xs)
  (setf-cof nym)
  (csyn '***curr-class*** nym)
  (format nil "struct ~a~a" nym (if xs (block-c xs) "")))

(defun/c virtual (&optional x y)
  (setf-cof x)
  (format nil "virtual ~a~a" x
          (if y (format nil " = ~a" (compile-form y)) "")))

(defun/c deprecated (&optional x &rest msg)
  (setf-cof x)
  (format nil "[[deprecated~a]] ~a"
          (if msg (format nil "(\"~{~a~^ ~}\")" (mapcar #'compile-form msg)) "")
          x))

(defun/c delete (&optional x)
  (setf-cof x)
  (format nil "delete ~a" x))

(defun/c lambda++ (&optional capture-list params attribs ret &rest body)
  (when (eq capture-list '[])
      (setf capture-list nil))
  (setf capture-list (mapcar
                      #'(lambda (x)
                          (if (atom x)
                              (c-strify x t)
                              (compile-form x)))
                      (fold/list capture-list)))
  (setf attribs (mapcar #'compile-form (fold/list attribs)))
  (format nil "[~{~a~^,~}]~a~{~^ ~a~}~a~a" capture-list
          (if (or params attribs ret) (parenify (str<- (vars-c params #\, nil))) "")
          attribs
          (if ret (format nil " -> ~a " (compile-form ret)) "")
          (block-c body)))

(defun/c lambda++* (&optional args &rest body)
  (apply #'lambda++-c (append (pad-right (fold/list args) nil 4) body)))

(defun/c namespace (&rest terms)
  (setf-cofs terms)
  (format nil "~{~a~^~(::~)~}" terms))

(defun/c namespacedecl (nym &rest terms)
  (setf-cof nym)
  (format nil "namespace ~a~a" nym (block-c terms)))

(defun/c typ& (&optional nym (n 1) const)
  (setf-cof nym)
  (unless (numberp n)
    (setf n 1)
    (setf const 'const))
  (format nil "~a~a~a"
          nym
          (if const (format nil " ~a" (compile-form const)) "")
          (str<repeat-n< #\& n)))

(defun/c ptr& (&optional nym (n 1))
  (setf-cof nym)
  (format nil "~a~a" (str<repeat-n< #\& n) nym))

(defun/c typ[&] (&optional nym (n 1))
  (setf-cof nym)
  (format nil "~a(~a)" nym (str<repeat-n< #\& n)))

(defun/c ptr[&] (&optional nym (n 1))
  (setf-cof nym)
  (format nil "(~a)~a" (str<repeat-n< #\& n) nym))

(defun/c class (&optional nym &rest terms)
  (setf-cof nym)
  (csyn '***curr-class*** nym)
  (when (and (listp (car terms))
             (member (caar terms) '(inherits inh)))
    (setf nym (format nil "~a : ~{~a~^ ~}" nym
                      (mapcar #'compile-form (cdar terms))))
    (setf terms (cdr terms)))
  (format nil "class~a~a~a" (if nym " " "") nym (if terms (block-c terms) "")))

(defun/c protected (&rest terms)
  (setf-cofs terms)
  (format nil "protected:~%~a" (block-c terms nil)))

(defun/c private (&rest terms)
  (setf-cofs terms)
  (format nil "private:~%~a" (block-c terms nil)))

(defun/c public (&rest terms)
  (setf-cofs terms)
  (format nil "public:~%~a" (block-c terms nil)))

(defun/c construct (&optional args init-pairs &rest code)
  (format nil "~a(~a)~a~a" (compile-form '***curr-class***) (vars-c args)
          (if init-pairs
              (format nil " : ~{~a~^~(, ~)~}"
                      (mapcar
                       #'(lambda (xs)
                           (format nil "~a(~a)" (compile-form (car xs))
                                   (if (cadr xs)
                                       (compile-form (cadr xs))
                                       (compile-form (car xs)))))
                       init-pairs))
              "")
          (if code (block-c code) "")))

(defun/c destroy (&optional args &rest code)
  (format nil "~~~a(~a)~a" (compile-form '***curr-class***) (vars-c args)
          (if code (block-c code) "")))

(defun/c constructor ()
  (format nil "~a" (compile-form '***curr-class***)))

(defun/c destructor ()
  (format nil "~~~a" (compile-form '***curr-class***)))

(defun/c suffix (x y)
  (format nil "~a~a" (compile-form x) (c-strify y)))

(defun/c operator (oper &optional typ args &rest code)
  (let ((opr "operator")
        (constif ""))
    (when (and (listp oper)
               (member (car oper) '(s su suf suffix)))
      (setf oper (format nil "\"\"_~a" (c-strify (cadr oper)))))
    (setf-cof typ)
    (when (and (listp oper)
               (member (car oper) '(@ ns namespace n/c)))
      (setf opr (apply #'namespace-c
                       (append (butlast (cdr oper)) (list opr))))
      (setf oper (car (last oper))))
    (when (null oper)
      (setf oper "()"))
    (setf oper (c-strify oper t))
    (when (eq (car code) 'const)
      (setf constif " const ")
      (setf code (cdr code)))
    (format nil "~a ~a~a~a(~a)~a~a" typ opr
            (if (a-Z? (char (str<- oper) 0)) " " "")
            oper (vars-c args) constif
            (if code (block-c code) ""))))

(defun/c friend (code)
  (setf-cof code)
  (format nil "friend ~a" code))

(defun/c decltemp (&optional var typ &rest code)
  (if (listp var)
      (progn (setf var (mapcar #'fold/list var))
             (setf code (cons typ code)))
      (setf var (fold/list/n (list (list var typ)))))
  (setf-cof typ)
  (setf var (format nil "~{~a~^,~}"
                    (mapcar
                     #'(lambda (pair)
                         (format nil "~{~a~^ ~}"
                                 (reverse (mapcar #'compile-form pair))))
                     var)))
  (format nil "template ~a~{~^ ~a~}"
          (if (or typ var) (format nil "<~a>" var) "<>")
          (if code (mapcar #'compile-form code) '(""))))

(defun/c temp (&optional var &rest typs)
  (setf-cof var)
  (setf-cofs typs)
  (format nil "~a<~{~a~^,~}>" var typs))

(defun/c using (namespace)
  (format nil "using namespace ~a" (compile-form namespace)))

(defun/c usevar (&rest args)
  (format nil "~a" (apply #'var-c (car args) 'using (cdr args))))

(defun/c comment++ (&rest comments)
  (setf-cofs comments)
  (format nil "//~{~a~^ ~}" comments))

(defun/c new (&rest xs)
  (setf-cofs xs)
  (format nil "new ~{~a~}" xs))

(defun/c try/catch (catch &optional trybody catchbody)
  (setf catch (apply #'var-c (fold/list catch)))
  (format nil "try~acatch(~a)~a" (block-c (fold/list trybody)) catch
          (if catchbody
              (block-c (fold/list catchbody))
              "")))

(defun/c strlit (&rest xs)
  (format nil "~a" (apply #'str-c xs)))

(defun/c explicit (&rest xs)
  (setf-cofs xs)
  (format nil "explicit ~{~a~}" xs))

(macropairs                             ; Defines synonyms.
 cfunc-syn
 func          f{} ; expands to: (progn (defun f{}-c (&rest args) (apply #'func-c args)) (compile 'f{}-c))
 funcarg       arg{}
 funcarg       fa{}
 namespace     n/s
 namespace     ns
 namespace     @
 slot          ->
 mem           .>
 typ*          t*
 typ&          t&
 typ[&]        [t&]
 typ[&]        t[&]
 typ[&]        t&[]
 ptr           p*
 ptr&          p&
 ptr[&]        [p&]
 ptr[&]        p[&]
 ptr[&]        p&[]
 ptr&          var&
 var           v
 delete        del
 class         c.
 class         d/c
 operator      op
 operator      opr
 construct     cx
 constructor   cxr
 destroy       dx
 destructor    dxr
 virtual       vxl
 virtual       virt
 virtual       virt.
 return        r
 headers       hh
 headers++     h+
 header        h
 typedef       t/d
 nth           n.
 nth           no.
 nth           nn
 arr           ar
 arr-decl      {}s
 main          m
 while         w
 do-while      d/w
 for           f
 arr           a.
 char          ch
 str           s.
 varlist       v/l
 switch        sw
 call          c
 struct        s{}
 struct        sx
 struct-decl   sd{}
 struct-decl   s{}s
 struct-decl   {sd}
 struct++      s{}+
 struct++      s{+}
 struct++      sx+
 struct++      sx++
 struct++      struct+
 block         b
 define        d#
 pragma        p#
 public        pu.
 private       pr.
 protected     px.
 friend        fr.
 template      tmplt
 template      !!
 templates     !!!
 template      t.
 templates     t..
 lispmacro     l/m
 lispmacro      !!l
 lisp/c-macro  l/c-macro
 lisp/c-macro  l/c/m
 lisp/c-macro  !!lc
 camelcase     camel
 lcamelcase    lcamel
 capitalize    cap
 uncapitalize  !cap
 lowercase     lcase
 uppercase     ucase
 dashify       -ify
 comment       cmt
 comment       z
 comment       /*
 comment++     cmt+
 comment++     cmt++
 comment++     z+
 comment++     //
 temp          <>
 decltemp      <t>
 decltemp      t<>
 <<+           <stream
 <<+           <<stream
 <<+           <stream<
 <<+           stream<
 <<+           stream<<
 <<+           <<<
 >>+           stream>
 >>+           stream>>
 >>+           >stream
 >>+           >>stream
 >>+           >>>
 addr          memloc
 addr          loc
 try/catch     t/c
 using         u.
 usevar        uv
 usevar        use
 namespacedecl ns/d
 namespacedecl n/s/d
 namespacedecl ns{}
 namespacedecl n/s{}
 tridot        t---
 tridot        t...
 tridot        d...
 tridot        v...
 lambda++      l++
 lambda++      l+
 lambda++      l[]
 lambda++      lambda[]
 lambda++      lambda+
 lambda++*     l++*
 lambda++*     l+*
 lambda++*     l*
 lambda++*     l[]*
 lambda++*     lambda[]*
 lambda++*     lambda+*
 lambda++*     lambda*
 )

;;; SYNONYMS
(flet ((set-c-synonym (pair)
         (let ((key (nth 0 pair))
               (val (nth 1 pair)))
           (setf (gethash key *c-synonyms*) val))))
  (mapcar #'set-c-synonym
          '(
            ;; CUDA STUFF
            (cuda/malloc            "cudaMalloc")
            (cuda/memcpy            "cudaMemcpy")
            (cuda/free              "cudaFree")
            (cuda/host->dev         "cudaMemcpyHostToDevice")
            (cuda/dev->host         "cudaMemcpyDeviceToHost")
            (cuda/dev/count         "cudaDeviceCount")
            (cuda/dev/set           "cudaSetDevice")
            (cuda/dev/get           "cudaGetDevice")
            (cuda/dev/props         "cudaDeviceProperties")
            (cuda/sync              "__syncthreads")
            (block/idx              "blockIdx")
            (block/idx/x            "blockIdx.x")
            (block/idx/y            "blockIdx.y")
            (block/idx/z            "blockIdx.z")
            (thread/idx             "threadIdx")
            (thread/idx/x           "threadIdx.x")
            (thread/idx/y           "threadIdx.y")
            (thread/idx/z           "threadIdx.z")
            (block/dim              "blockDim")
            (block/dim/x            "blockDim.x")
            (block/dim/y            "blockDim.y")
            (block/dim/z            "blockDim.z")
            (grid/dim               "gridDim")
            (grid/dim/x             "gridDim.x")
            (grid/dim/y             "gridDim.y")
            (grid/dim/z             "gridDim.z")
            (dim/block              "dimBlock")
            (dim/grid               "dimGrid")

            ;; MPI STUFF
            (mpi/success            "MPI_SUCCESS")
            (mpi/err/buffer         "MPI_ERR_BUFFER")
            (mpi/err/count          "MPI_ERR_COUNT")
            (mpi/err/type           "MPI_ERR_TYPE")
            (mpi/err/tag            "MPI_ERR_TAG")
            (mpi/err/comm           "MPI_ERR_COMM")
            (mpi/err/rank           "MPI_ERR_RANK")
            (mpi/err/request        "MPI_ERR_REQUEST")
            (mpi/err/root           "MPI_ERR_ROOT")
            (mpi/err/group          "MPI_ERR_GROUP")
            (mpi/err/op             "MPI_ERR_OP")
            (mpi/err/topology       "MPI_ERR_TOPOLOGY")
            (mpi/err/dims           "MPI_ERR_DIMS")
            (mpi/err/arg            "MPI_ERR_ARG")
            (mpi/err/unknown        "MPI_ERR_UNKNOWN")
            (mpi/err/truncate       "MPI_ERR_TRUNCATE")
            (mpi/err/other          "MPI_ERR_OTHER")
            (mpi/err/intern         "MPI_ERR_INTERN")
            (mpi/pending            "MPI_PENDING")
            (mpi/err/in/status      "MPI_ERR_IN_STATUS")
            (mpi/err/lastcode       "MPI_ERR_LASTCODE")
            (mpi/bottom             "MPI_BOTTOM")
            (mpi/proc/null          "MPI_PROC_NULL")
            (mpi/any/source         "MPI_ANY_SOURCE")
            (mpi/any/tag            "MPI_ANY_TAG")
            (mpi/undefined          "MPI_UNDEFINED")
            (mpi/bsend/overhead     "MPI_BSEND_OVERHEAD")
            (mpi/keyval/invalid     "MPI_KEYVAL_INVALID")
            (mpi/errors/are/fatal   "MPI_ERRORS_ARE_FATAL")
            (mpi/errors/return      "MPI_ERRORS_RETURN")
            (mpi/max/processor/name "MPI_MAX_PROCESSOR_NAME")
            (mpi/max/error/string   "MPI_MAX_ERROR_STRING")
            (mpi/char               "MPI_CHAR")
            (mpi/short              "MPI_SHORT")
            (mpi/int                "MPI_INT")
            (mpi/long               "MPI_LONG")
            (mpi/unsigned/char      "MPI_UNSIGNED_CHAR")
            (mpi/unsigned/short     "MPI_UNSIGNED_SHORT")
            (mpi/unsigned           "MPI_UNSIGNED")
            (mpi/unsigned/long      "MPI_UNSIGNED_LONG")
            (mpi/float              "MPI_FLOAT")
            (mpi/double             "MPI_DOUBLE")
            (mpi/long/double        "MPI_LONG_DOUBLE")
            (mpi/byte               "MPI_BYTE")
            (mpi/packed             "MPI_PACKED")
            (mpi/float/int          "MPI_FLOAT_INT")
            (mpi/double/int         "MPI_DOUBLE_INT")
            (mpi/long/int           "MPI_LONG_INT")
            (mpi/2int               "MPI_2INT")
            (mpi/short/int          "MPI_SHORT_INT")
            (mpi/long/double/int    "MPI_LONG_DOUBLE_INT")
            (mpi/long/long/int      "MPI_LONG_LONG_INT")
            (mpi/ub                 "MPI_UB")
            (mpi/lb                 "MPI_LB")
            (mpi/comm/world         "MPI_COMM_WORLD")
            (mpi/comm/self          "MPI_COMM_SELF")
            (mpi/ident              "MPI_IDENT")
            (mpi/congruent          "MPI_CONGRUENT")
            (mpi/similar            "MPI_SIMILAR")
            (mpi/unequal            "MPI_UNEQUAL")
            (mpi/tag/ub             "MPI_TAG_UB")
            (mpi/io                 "MPI_IO")
            (mpi/host               "MPI_HOST")
            (mpi/wtime/is/global    "MPI_WTIME_IS_GLOBAL")
            (mpi/max                "MPI_MAX")
            (mpi/min                "MPI_MIN")
            (mpi/sum                "MPI_SUM")
            (mpi/prod               "MPI_PROD")
            (mpi/maxloc             "MPI_MAXLOC")
            (mpi/minloc             "MPI_MINLOC")
            (mpi/band               "MPI_BAND")
            (mpi/bor                "MPI_BOR")
            (mpi/bxor               "MPI_BXOR")
            (mpi/land               "MPI_LAND")
            (mpi/lor                "MPI_LOR")
            (mpi/lxor               "MPI_LXOR")
            (mpi/group/null         "MPI_GROUP_NULL")
            (mpi/comm/null          "MPI_COMM_NULL")
            (mpi/datatype/null      "MPI_DATATYPE_NULL")
            (mpi/request/null       "MPI_REQUEST_NULL")
            (mpi/op/null            "MPI_OP_NULL")
            (mpi/errhandler/null    "MPI_ERRHANDLER_NULL")
            (mpi/group/empty        "MPI_GROUP_EMPTY")
            (mpi/graph              "MPI_GRAPH")
            (mpi/cart               "MPI_CART")
            (mpi/aint               "MPI_Aint")
            (mpi/status             "MPI_Status")
            (mpi/group              "MPI_Group")
            (mpi/comm               "MPI_Comm")
            (mpi/datatype           "MPI_Datatype")
            (mpi/request            "MPI_Request")
            (mpi/op                 "MPI_Op")
            (mpi/status/ignore      "MPI_STATUS_IGNORE")
            (mpi/statuses/ignore    "MPI_STATUSES_IGNORE")
            (mpi/copy/function      "MPI_Copy_function")
            (mpi/delete/function    "MPI_Delete_function")
            (mpi/handler/function   "MPI_Handler_function")
            (mpi/user/function      "MPI_User_function")
            (mpi/init               "MPI_Init")
            (mpi/send               "MPI_Send")
            (mpi/recv               "MPI_Recv")
            (mpi/bcast              "MPI_Bcast")
            (mpi/comm/size          "MPI_Comm_size")
            (mpi/comm/rank          "MPI_Comm_rank")
            (mpi/abort              "MPI_Abort")
            (mpi/get/processor/name "MPI_Get_processor_name")
            (mpi/get/version        "MPI_Get_version")
            (mpi/initialized        "MPI_Initialized")
            (mpi/wtime              "MPI_Wtime")
            (mpi/wtick              "MPI_Wtick")
            (mpi/finalize           "MPI_Finalize")
            (mpi/open/port          "MPI_Open_port")
            (mpi/comm/accept        "MPI_Comm_accept")
            (mpi/comm/connect       "MPI_Comm_connect")
            (mpi/scan               "MPI_Scan")
            (mpi/allreduce          "MPI_Allreduce")
            (mpi/comm/split         "MPI_Comm_split")
            (mpi/isend              "MPI_Isend")
            (mpi/irecv              "MPI_Irecv")
            (mpi/wait               "MPI_Wait")
            (mpi/test               "MPI_Test")
            (mpi/init               "MPI_Init")
            (mpi/finalize           "MPI_Finalize")
            (mpi/comm/rank          "MPI_Comm_rank")
            (mpi/comm/size          "MPI_Comm_size")
            (mpi/get/count          "MPI_Get_count")
            (mpi/type/extent        "MPI_Type_extent")
            (mpi/type/struct        "MPI_Type_struct")
            (mpi/scatter            "MPI_Scatter")
            (mpi/gather             "MPI_Gather")
            (mpi/sendrecv           "MPI_Sendrecv")
            (mpi/sendrecv/replace   "MPI_Sendrecv_replace")
            (mpi/group/rank         "MPI_Group_rank")
            (mpi/group/size         "MPI_Group_size")
            (mpi/comm/group         "MPI_Comm_group")
            (mpi/group/free         "MPI_Group_free")
            (mpi/group/incl         "MPI_Group_incl")
            (mpi/comm/create        "MPI_Comm_create")
            (mpi/wtime              "MPI_Wtime")
            (mpi/get/processor/name "MPI_Get_processor_name")

            ;; PTHREADS API STUFF
            (pthread/create              "pthread_create")
            (pthread/equal               "pthread_equal")
            (pthread/exit                "pthread_exit")
            (pthread/join                "pthread_join")
            (pthread/self                "pthread_self")
            (pthread/mutex/init          "pthread_mutex_init")
            (pthread/mutex/destroy       "pthread_mutex_destroy")
            (pthread/mutex/lock          "pthread_mutex_lock")
            (pthread/mutex/trylock       "pthread_mutex_trylock")
            (pthread/mutex/unlock        "pthread_mutex_unlock")
            (pthread/cond/init           "pthread_cond_init")
            (pthread/cond/destroy        "pthread_cond_destroy")
            (pthread/cond/wait           "pthread_cond_wait")
            (pthread/cond/timedwait      "pthread_cond_timedwait")
            (pthread/cond/signal         "pthread_cond_signal")
            (pthread/cond/broadcast      "pthread_cond_broadcast")
            (pthread/once                "pthread_once")
            (pthread/key/create          "pthread_key_create")
            (pthread/key/delete          "pthread_key_delete")
            (pthread/setspecific         "pthread_setspecific")
            (pthread/getspecific         "pthread_getspecific")
            (pthread/cleanup/push        "pthread_cleanup_push")
            (pthread/cleanup/pop         "pthread_cleanup_pop")
            (pthread/attr/init           "pthread_attr_init")
            (pthread/attr/destroy        "pthread_attr_destroy")
            (pthread/attr/getstacksize   "pthread_attr_getstacksize")
            (pthread/attr/setstacksize   "pthread_attr_setstacksize")
            (pthread/attr/getdetachstate "pthread_attr_getdetachstate")
            (pthread/attr/setdetachstate "pthread_attr_setdetachstate")
            (flockfile                   "flockfile")
            (ftrylockfile                "ftrylockfile")
            (funlockfile                 "funlockfile")
            (getc/unlocked               "getc_unlocked")
            (getchar/unlocked            "getchar_unlocked")
            (putc/unlocked               "putc_unlocked")
            (putc/unlocked               "putc_unlocked")
            (pthread/detach              "pthread_detach")
            (pthread/threads/max         "PTHREAD_THREADS_MAX")
            (pthread/keys/max            "PTHREAD_KEYS_MAX")
            (pthread/stack/min           "PTHREAD_STACK_MIN")
            (pthread/create/detached     "PTHREAD_CREATE_DETACHED")
            (pthread/create/joinable     "PTHREAD_CREATE_JOINABLE")

            ;; BASIC STUFF
            (null                        "NULL")
            (arg/c                       "argc")
            (arg/count                   "argc")
            (arg/v                       "argv")
            (arg/values                  "argv")
            (size/t                      "size_t")
            (integer                     "int")
            (integer+                    "long")
            (natural                     "unsigned int")
            (natural+                    "unsigned long")
            (real                        "float")
            (real+                       "double")
            (boolean                     "char")
            (stringc                     "char*")
            (---                         "...")
            (-#                          "#")
            (-##                         "##")
            (-va-args-                   "__VA_ARGS__")
            (-empty-                     " ")
            (--                          " ")
            (-                           "_")
            ($                           nil)
            (int+                        "long int")
            (int++                       "long long int")
            (double+                     "long double")
            (float+                      "double")
            (float++                     "long double")
            (template-params             "template-params"))))

(defun c-code<-file (filename)
  "Return the compiled C code from the lispc code in FILENAME as a string."
  (let* ((s (file-string filename))
         (result t)
         (n 0)
         (cl-codes (loop :while result
                         :collect (progn
                                    (multiple-value-setq (result n)
                                      (read-from-string s nil))
                                    (setf s (subseq s n))
                                    result))))
    ;; (format nil "/* Compile Time: ~a */~%~a" (timestamp) (apply #'c cl-codes))
    (format nil "~a" (apply #'c cl-codes))))

(defun c-file<-cl-file (filein &optional fileout)
  "Entry point to compiling a .cl file to a .c file."
  (let ((s (c-code<-file filein))
        (temp nil))
    (when (null fileout)
      (setf temp filein)
      (setf fileout (file-ext temp 'c)))
    (when s
      (with-open-file (c-file-stream fileout :direction :output :if-does-not-exist :create)
        (format c-file-stream "~a" s)))))

(defun c-cl-file-continuous (filein &optional fileout ignore-error? (interval 1))
  (format t "Press ^C to stop.")
  (do ((i 0 (+ i interval))) (nil)
    (progn
      (format t "~&~a" (elapsed-time i))
      (if ignore-error?
          (ignore-errors
           (c-file<-cl-file filein fileout))
          (c-file<-cl-file filein fileout))
      (sleep interval))))

(defun compile-cl-file (filein &key fileout tags libs c-file cc)
  (def fileout "a.out")
  (def tags "")
  (def libs "")
  (def cc "gcc")
  (let ((c-code (c-code<-file filein)) (temp-file (if c-file c-file (temp-filename ".c"))))
    (format t "~a" c-file)
    (if (and *last-compiled* (not (eq *last-compiled* c-file))) (delete-file *last-compiled*))
    (with-open-file (c-file-stream temp-file :direction :output :if-does-not-exist :create)
      (format c-file-stream "~a" c-code))
    (format t "Running: ~a ~a ~a -o ~a ~a~%" cc tags temp-file fileout libs)
    (uiop:run-program (format nil "~a ~a ~a -o ~a ~a" cc tags temp-file fileout libs))
    (setf *last-compiled* temp-file)))

(defun compile-and-run-cl-file (filein &key args fileout tags libs c-file cc)
  (def fileout "a.out")
  (compile-cl-file filein
                   :fileout fileout
                   :tags tags
                   :libs libs
                   :c-file c-file
                   :cc cc)
  (format t "Running: ./~a~{~^ ~a~}~%" fileout args)
  (uiop:run-program (format nil "./~a~{~^ ~a~}" fileout args)))
