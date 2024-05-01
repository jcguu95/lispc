;;;; Copyright Jonathan Baca, 2016

;; (defvar *lbrac* #\[)
(defvar *file-out*      nil)
(defvar *exec-out*      nil)
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

(defun compile-c ()
  (uiop:run-program (format nil "gcc ~a -o ~a" *file-out* *exec-out*)))

(defmacro decr (x)
  `(setf ,x (1- ,x)))

(defun c-strify (x &optional leave-mostly-alone)
  (unless (stringp x) (setf x (str<- x)))
  (cond (leave-mostly-alone
         (just-downcase x))
        ((numeric-string? x)
         x)
        ((eq (char x 0) #\!)
         (replace-char #\- #\_ (cof (subseq x 1))))
        ((eq (char x 0) #\=)
         (camelcase-c (cof (subseq x 1))))
        ((eq (char x 0) #\-)
         (Lcamelcase-c (cof (subseq x 1))))
        (t
         (replace-char #\- #\_ (string-downcase x)))))

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
                 `(format nil "(~a)" (cof ,b))
                 `(cof ,b))))
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
               (format nil "~a~a~a" ,lp (cof (car xs)) ,rp)
               (format nil "~a~a~a~a~a~a~a"
                       ,lp
                       ,lp (cof (car xs)) ,rp
                       ',oper
                       (apply (function ,(symbol-append-c nym)) (cdr xs)) ,rp))))))

(defmacro rredop (oper &key nym nparen)
  (def nym oper)
  (let ((lp (if nparen "" "("))
        (rp (if nparen "" ")")))
    `(defun/c ,nym (&rest xs)
       (if (null xs) nil
           (if (= 1 (length xs))
               (format nil "~a~a~a" ,lp (cof (car xs)) ,rp)
               (format nil "~a~a~a~a~a~a~a" ,lp
                       (apply (function ,(symbol-append-c nym)) (butlast xs))
                       ',oper ,lp (cof (car (last xs))) ,rp ,rp))))))

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
             ,(if nparen "" "(") (cof x)
             ,(if nparen "" ")") )))

(defmacro post (oper &key nym nparen)
  `(defun/c ,nym (x)
     (format nil "~a~a~a~a"
             ,(if nparen "" "(") (cof x)
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
  (format nil "~{~a~^~(;~%~%~)~}" (mapcar #'cof xs)))

;; (defun pc (&rest xs)
;;   (format t "~a" (apply #'c xs)))

(defmacro cwrite (&rest xs)
  `(write-out (format nil "~a;~%" (c ,@xs))))

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
  (format nil "~{~a~^-~}" (mapcar #'cof strs)))

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

(defun cof (x)
  (if (null x)
      ""
      (if (atom x)
          (if (inhash x *c-synonyms*)
              (cof (gethash x *c-synonyms*))
              (c-strify x))
          (if (atom (car x))
              (if (and
                   (> (length (str<- (car x))) 1)
                   (not (fboundp (symbol-append-c (car x)))))
                  (case (char (str<- (car x)) 0)
                    (#\@ (apply #'call-c (cof (trim-symbol (car x) 1)) (cdr x)))
                    (#\[ (apply #'nth-c (cof (trim-symbol (car x) 2)) (cdr x)))
                    (#\] (apply #'arr-c (cof (trim-symbol (car x) 1)) (cdr x)))
                    (#\& (apply #'addr-c (cof (trim-symbol (car x) 1)) (cdr x)))
                    (#\^ (apply #'cast-c (cof (trim-symbol (car x) 1)) (cdr x)))
                    (#\* (apply #'ptr-c (cof (trim-symbol (car x) 1)) (cdr x)))
                    (#\. (apply #'mem-c (cof (trim-symbol (car x) 1)) (cdr x)))
                    (#\> (apply #'slot-c (cof (trim-symbol (car x) 1)) (cdr x)))
                    (#\= (apply #'camelcase-c (str<- (trim-symbol (car x) 1)) (mapcar #'str<- (cdr x))))
                    (#\% (apply #'lcamelcase-c (str<- (trim-symbol (car x) 1)) (mapcar #'str<- (cdr x))))
                    (#\- (apply #'lcamelcase-c (str<- (trim-symbol (car x) 1)) (mapcar #'str<- (cdr x))))
                    (otherwise (apply (symbol-append-c (car x)) (cdr x))))
                  (apply (symbol-append-c (car x)) (cdr x)))
              (format nil "~{~a~^~(;~%~)~}" (mapcar #'cof x))))))

(defmacro cofy (x) `(setf ,x (cof ,x)))
(defmacro cofsy (x) `(setf ,x (mapcar #'cof (fold/list ,x))))

;;; DEFINE THE C LANGUAGE

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
                            .und-eq bit-y-eq by-eq .y-eq &= bit-and= band= .and= bit-et= bet=
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
        (>>= :l t :nyms (>>= r-shift-eq shift-right-eq shr-eq >>= r-shift= shift-right= shr=))
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
  (format nil "{~{~a~^~(, ~)~}}" (mapcar #'cof xs)))

(defun/c struct-decl (&optional nym &rest xs)
  (cofy nym)
  (format nil "(~a){~{~a~^~(, ~)~}}" nym (mapcar #'cof xs)))


(defun/c sym/add (&rest xs) (cofsy xs)
  (str<-lst xs))


(defun/c slot (a &rest bs)
  (cofy a)
  (cofsy bs)
  (format nil "(~a)~a~{~a~^~(->~)~}" a
          (if bs
              "->"
              "")
          bs))

(defun/c mem (a &rest bs)
  (cofy a)
  (cofsy bs)
  (format nil "(~a)~a~{~a~^.~}" a
          (if bs
              "."
              "")
          bs))

(defun/c typ* (x &optional (n 1))
  (cofy x)
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
  (format nil "~{  ~a;~^~%~}" (mapcar #'cof xs)))

(defun/c ? (test ifyes ifno)
  (cofy test)
  (cofy ifyes)
  (cofy ifno)
  (format nil "(~a)?~a:(~a)" test ifyes ifno))

(defun/c if (test &optional ifyes ifno)
  (cofy test)
  (cofy ifyes)
  (format nil "if(~a) {~%   ~a;~%}~a" test ifyes
          (if ifno
              (format nil "else{~%   ~a;~%}" (cof ifno))
              "")))

(defun/c cond (&rest pairs)
  (format nil "if(~a) {~{~%  ~a;~}~%}~{~a~}" (cof (caar pairs))
          (mapcar #'cof (cdar pairs))
          (mapcar
           #'(lambda (pair)
               (format nil "else if(~a){~{~%   ~a;~}~%}" (cof (car pair))
                       (mapcar #'cof (cdr pair))))
           (cdr pairs))))

(defun/c ifs (&rest pairs)
  (format nil "if(~a) {~{~%  ~a;~}~%}~{~a~}" (cof (caar pairs))
          (mapcar #'cof (cdar pairs))
          (mapcar
           #'(lambda (pair)
               (format nil "if(~a){~{~%   ~a;~}~%}" (cof (car pair))
                       (mapcar #'cof (cdr pair))))
           (cdr pairs))))

(defun/c main (&rest body)
  (format nil "int main(int argc,char **argv)~a" (block-c body)))

(defun/c for (a b c &rest lines)
  (cofy a)
  (cofy b)
  (cofy c)
  (format nil "for(~a;~a;~a)~a" a b c (block-c lines)))

(defun/c while (test &rest lines)
  (cofy test)
  (format nil "while(~a) ~a" test (block-c lines)))

(defun/c do-while (test &rest lines)
  (cofy test)
  (format nil "do~awhile(~a)" (block-c lines) test))

(defun/c switch (var &rest pairs)
  (cofy var)
  (labels ((helper (pairs)
             (format nil "~a:~%   ~a~%~a" (cof (caar pairs))
                     (block-c (cdar pairs) nil)
                     (if (cdr pairs)
                         (helper (cdr pairs))
                         ""))))
    (format nil "switch(~a){~a}" var (helper pairs))))

(defun/c addr (x &optional (n 1))
  (cofy x)
  (format nil "~a(~a)" (str<repeat-n< #\& n) x))

(defun/c ptr (x &optional (n 1))
  (format nil "~{~a~}(~a)"
          (loop for i from 1 to n
                collect #\*)
          (cof x)))

(defun/c pt (x &optional (n 1))
  (format nil "~{~a~}~a"
          (loop for i from 1 to n
                collect #\*)
          (cof x)))

(defun/c nth (x &optional (n 0) &rest ns)
  (format nil "(~a)[~a]~a" (cof x) (cof n)
          (if ns
              (format nil "~{[~a]~}" (mapcar #'cof ns))
              "")))

(defun/c arr (x &optional n &rest ns)
  (format nil "~a[~a]~a" (cof x) (cof n)
          (if ns
              (format nil "~{[~a]~}" (mapcar #'cof ns))
              "")))

(defun/c call (nym &rest args)
  (format nil "~a(~{~a~^,~})" (cof nym) (mapcar #'cof args)))

(defun/c cuda/call (nym ijk &rest args)
  (cofy nym)
  (cofsy ijk)
  (format nil "~a<<<~{~a~^,~}>>>(~{~a~^,~})" nym ijk (mapcar #'cof args)))

(defun/c str (&rest x)
  (cofsy x)
  (format nil "\"~{~a~^ ~}\"" x))

(defun/c char (x)
  (cofy x)
  (format nil "'~a'" x))

(defun/c cast (nym &optional (typ 'int) &rest typs)
  (if typs
      (apply #'cast-c (cast-c nym typ) typs)
      (format nil "((~a)(~a))" (cof typ) (cof nym))))

(defun/c var (x &optional type init &rest modifiers)
  (cofy x)
  (cofy type)
  (format nil "~a~a~{~a~^,~}~a"
          (if modifiers
              (format nil "~{~a ~}" (mapcar #'cof modifiers))
              "")
          (if type
              (format nil "~a " type)
              "")
          (fold/list x)
          (if init
              (format nil "=~a" (cof init))
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

(defun/c struct (nym &optional vars) (cofy nym)
  (csyn '***curr-class*** nym)
  (if vars
      (format nil "struct ~a{~%  ~a;~%}" nym (vars-c vars #\;))
      (format nil "struct ~a" nym)))

(defun/c union (nym &optional vars)
  (cofy nym)
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
      (setf preq (format nil "~a -> ~a" preq (cof (cadar lines))))
      (setf lines (cdr lines)))
    (format nil "~a~a~a~{   ~a~(;~%~)~}~a" preq
            (if bracket #\{ "")
            (if unempty #\Newline "")
            (if unempty (mapcar #'cof (fold/list lines)) nil)
            (if bracket #\} ""))))

(defun/c func (nym &optional typ vars &rest body)
  (cofy nym)
  (cofy typ)
  (format nil "~a ~a(~a)~a" typ nym (vars-c vars #\, nil)
          (if body (block-c body) "")))

(defun/c inline (arg)
  (format nil "inline ~a" (cof arg)))

(defun/c cuda/global (&rest args)
  (format nil "__global__ ~a" (apply #'func-c args)))

(defun/c cuda/device (&rest args)
  (format nil "__device__ ~a" (apply #'func-c args)))

(defun/c funcarg (nym typ &optional varforms)
  (cofy nym)
  (cofy typ)
  (cofsy varforms)
  (format nil "~a(*~a)(~{~a~^,~})" typ nym varforms))

(defun/c return (&optional x &rest ys)
  (cofy x)
  (format nil "return ~a~a~{~^ ~a~}" x
          (if ys
              #\;
              "")
          (if ys
              (mapcar #'cof ys)
              nil)))

(defun/c typedef (x &optional y)
  (cofy x)
  (format nil "typedef ~a ~a;~%" x
          (if y
              (cof y)
              "")))

(defun/c enum (nym &rest mems)
  (cofy nym)
  (cofsy mems)
  (format nil "enum ~a{~{~a~^~(, ~)~}};~%" nym mems))

(defun/c h-file (nym)
  (cofy nym)
  (format nil "~a.h" nym))

(defun/c str/add (&rest xs)
  (format nil "~{~a~}" (cof xs)))

(defun/c include (filename &key local)
  (cofy filename)
  (format nil "#include ~a~a~a~%"
          (if local
              #\"
              #\<)
          filename
          (if local
              #\"
              #\>)))

(defun/c import (filename)
  (setf filename
        (if (stringp filename)
            filename
            (format nil "~a.cl" (cof filename))))
  (c-code<-file filename)
  (format nil "/* ~a LOADED */" filename))

(defun/c macro (nym &rest xs)
  (cofy nym)
  (format nil "~a(~{~a~^,~})" nym (mapcar #'cof (fold/list xs))))

(defun/c unsigned (x)
  (cofy x)
  (format nil "unsigned ~a" x))

(defun/c define (a b)
  (cofy a)
  (cofy b)
  (format nil "#define ~a ~a~%" a b))

(defun/c ifdef (expr)
  (cofy expr)
  (format nil "#ifdef ~~%" expr))

(defun/c ifndef (expr)
  (cofy expr)
  (format nil "#ifndef ~~%" expr))

(defun/c |IF#| (expr)
  (cofy expr)
  (format nil "#if ~a~%" expr))

(defun/c |ELSE#| nil
  "#else~%")

(defun/c endif nil
  "#endif~%")

(defun/c pragma (&rest xs)
  (cofsy xs)
  (format nil "#pragma ~{~a~^ ~}" xs))

(defun/c paren (x)
  (cofy x)
  (format nil "(~a)" x))

(defun/c comment (&rest xs)
  (let* ((small (eq (car xs) 's))
         (s
           (format nil "/* ~{~a~^ ~} */~%"
                   (mapcar #'cof
                           (if small
                               (cdr xs)
                               xs))))
         (v
           (if small
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
  (cofsy xs)
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
  (cof (eval `(apply (replacify-lambda ,llist ,template) ',args))))

(defun/c template (f vars template)
  (eval `(defun/c ,f (&rest args)
           (cof (apply (replacify-lambda ,vars ,template) (mapcar #'cof args)))))
  (sethash f t *template-list*)
  (format nil "/**DEFINED: \"~a\" (template)**/" f))

(defun/c templates (f vars template)
  (eval
   `(defun/c ,f (&rest argss)
      (apply #'progn-c
             (mapcar #'cof
                     (mapcar
                      #'(lambda (args)
                          (apply (replacify-lambda ,vars ,template)
                                 (mapcar #'cof (fold/list args))))
                      argss)))))
  "")

(defun/c cuda/dim3 (typ x y)
  (cofy typ)
  (cofy x)
  (cofy y)
  (format nil "dim3 ~a(~a,~a)" typ x y))

(defun/c cuda/dim/block (x y)
  (cuda/dim3-c 'dim/block x y))

(defun/c cuda/dim/grid (x y)
  (cuda/dim3-c 'dim/grid x y))

(defun/c cuda/shared (&rest xs)
  (format nil "__shared__ ~a" (apply #'var-c xs)))

(defun/c repeat (x &optional (n 1))
  (cofy x)
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
  (cofsy xs)
  (format nil (format nil "(~~{(~~a)~~^~~(~a~~)~~})" opr) xs))

(defun/c funcall-if (test func &rest args)
  (if test
      (apply #'funcall-c func args)
      (str<-lst (mapcar #'cof args))))

(defun/c apply-if (test func args)
  (if test
      (apply #'funcall-c func args)
      (str<-lst (mapcar #'cof args))))

(defun/c test-eq (a b)
  (eq a b))

(defun/c test-not (a)
  (not a))

(defun/c test-and (&rest xs)
  (eval `(and ,@xs)))

(defun/c test-or (&rest xs)
  (eval `(or ,@xs)))

(defun/c code-list (&rest xs)
  (mapcar #'cof xs))

(defun/c list (&rest xs)
  xs)

;;; C++ Stuff
(defun/c hh-file (nym)
  (cofy nym)
  (format nil "~a.hh" nym))

(defun/c header++ (nym &key local)
  (if local
      (include-c (hh-file-c nym) :local local)
      (include-c nym)))

(defun/c headers++ (&rest xs)
  (format nil "~{~a~}"
          (mapcar #'(lambda (x) (apply #'header++-c (fold/list x))) xs)))

(defun/c tridot (x)
  (cofy x)
  (format nil "~a..." x))

(defun/c struct++ (&optional nym &rest xs)
  (cofy nym)
  (csyn '***curr-class*** nym)
  (format nil "struct ~a~a" nym (if xs (block-c xs) "")))

(defun/c virtual (&optional x y)
  (cofy x)
  (format nil "virtual ~a~a" x
          (if y (format nil " = ~a" (cof y)) "")))

(defun/c deprecated (&optional x &rest msg)
  (cofy x)
  (format nil "[[deprecated~a]] ~a"
          (if msg (format nil "(\"~{~a~^ ~}\")" (mapcar #'cof msg)) "")
          x))

(defun/c delete (&optional x)
  (cofy x)
  (format nil "delete ~a" x))

(defun/c lambda++ (&optional capture-list params attribs ret &rest body)
  (when (eq capture-list '[])
      (setf capture-list nil))
  (setf capture-list (mapcar
                      #'(lambda (x)
                          (if (atom x)
                              (c-strify x t)
                              (cof x)))
                      (fold/list capture-list)))
  (setf attribs (mapcar #'cof (fold/list attribs)))
  (format nil "[~{~a~^,~}]~a~{~^ ~a~}~a~a" capture-list
          (if (or params attribs ret) (parenify (str<- (vars-c params #\, nil))) "")
          attribs
          (if ret (format nil " -> ~a " (cof ret)) "")
          (block-c body)))

(defun/c lambda++* (&optional args &rest body)
  (apply #'lambda++-c (append (pad-right (fold/list args) nil 4) body)))

(defun/c namespace (&rest terms)
  (cofsy terms)
  (format nil "~{~a~^~(::~)~}" terms))

(defun/c namespacedecl (nym &rest terms)
  (cofy nym)
  (format nil "namespace ~a~a" nym (block-c terms)))

(defun/c typ& (&optional nym (n 1) const)
  (cofy nym)
  (unless ((numberp n))
    (setf n 1)
    (setf const 'const))
  (format nil "~a~a~a" nym
          (if const (format nil " ~a" (cof const)) "")
          (str<repeat-n< #\& n)))

(defun/c ptr& (&optional nym (n 1))
  (cofy nym)
  (format nil "~a~a" (str<repeat-n< #\& n) nym))

(defun/c typ[&] (&optional nym (n 1))
  (cofy nym)
  (format nil "~a(~a)" nym (str<repeat-n< #\& n)))

(defun/c ptr[&] (&optional nym (n 1))
  (cofy nym)
  (format nil "(~a)~a" (str<repeat-n< #\& n) nym))

(defun/c class (&optional nym &rest terms)
  (cofy nym)
  (csyn '***curr-class*** nym)
  (when (and (listp (car terms))
             (member (caar terms) '(inherits inh)))
    (setf nym (format nil "~a : ~{~a~^ ~}" nym
                      (mapcar #'cof (cdar terms))))
    (setf terms (cdr terms)))
  (format nil "class~a~a~a" (if nym " " "") nym (if terms (block-c terms) "")))

(defun/c protected (&rest terms)
  (cofsy terms)
  (format nil "protected:~%~a" (block-c terms nil)))

(defun/c private (&rest terms)
  (cofsy terms)
  (format nil "private:~%~a" (block-c terms nil)))

(defun/c public (&rest terms)
  (cofsy terms)
  (format nil "public:~%~a" (block-c terms nil)))

(defun/c construct (&optional args init-pairs &rest code)
  (format nil "~a(~a)~a~a" (cof '***curr-class***) (vars-c args)
          (if init-pairs
              (format nil " : ~{~a~^~(, ~)~}"
                      (mapcar
                       #'(lambda (xs)
                           (format nil "~a(~a)" (cof (car xs))
                                   (if (cadr xs)
                                       (cof (cadr xs))
                                       (cof (car xs)))))
                       init-pairs))
              "")
          (if code (block-c code) "")))

(defun/c destroy (&optional args &rest code)
  (format nil "~~~a(~a)~a" (cof '***curr-class***) (vars-c args)
          (if code (block-c code) "")))

(defun/c constructor ()
  (format nil "~a" (cof '***curr-class***)))

(defun/c destructor ()
  (format nil "~~~a" (cof '***curr-class***)))

(defun/c suffix (x y)
  (format nil "~a~a" (cof x) (c-strify y)))

(defun/c operator (oper &optional typ args &rest code)
  (let ((opr "operator")
        (constif ""))
    (when (and (listp oper)
               (member (car oper) '(s su suf suffix)))
      (setf oper (format nil "\"\"_~a" (c-strify (cadr oper)))))
    (cofy typ)
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
  (cofy code)
  (format nil "friend ~a" code))

(defun/c decltemp (&optional var typ &rest code)
  (if (listp var)
      (progn (setf var (mapcar #'fold/list var))
             (setf code (cons typ code)))
      (setf var (fold/list/n (list (list var typ)))))
  (cofy typ)
  (setf var (format nil "~{~a~^,~}"
                    (mapcar
                     #'(lambda (pair)
                         (format nil "~{~a~^ ~}"
                                 (reverse (mapcar #'cof pair))))
                     var)))
  (format nil "template ~a~{~^ ~a~}"
          (if (or typ var) (format nil "<~a>" var) "<>")
          (if code (mapcar #'cof code) '(""))))

(defun/c temp (&optional var &rest typs)
  (cofy var)
  (cofsy typs)
  (format nil "~a<~{~a~^,~}>" var typs))

(defun/c using (namespace)
  (format nil "using namespace ~a" (cof namespace)))

(defun/c usevar (&rest args)
  (format nil "~a" (apply #'var-c (car args) 'using (cdr args))))

(defun/c comment++ (&rest comments)
  (cofsy comments)
  (format nil "//~{~a~^ ~}" comments))

(defun/c new (&rest xs)
  (cofsy xs)
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
  (cofsy xs)
  (format nil "explicit ~{~a~}"))

(macropairs
 cfunc-syn
 func          f{} ; (progn (defun f{}-c (&rest args) (apply #'func-c args)) (compile 'f{}-c))
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
 switch        sx
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
