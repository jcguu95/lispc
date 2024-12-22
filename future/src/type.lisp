;; TODO Redo for the type system: Basic types are represented as keywords,
;; while composed types are represented as lists whose cars are of the
;; following: :pt, :fn, :struct, :array. The users can write
;; instead :pt, :fn, :{}, :[] respectively, but a normalizer will transform it
;; into the canonical, longer forms. Write util functions to handle and
;; inspect types. Allow users to define more type operators (e.g. :pt :fn as
;; above). Finally, write a printer, and integrate that with SET and DECLARE.
;; Mention that the type system is by no means complete, yet the user can
;; inline any C codes so that's not a problem.

;; TODO Change syntax from (:array () :int) to (:array :int)

(defun type? (form)
  (if (keywordp form)
      form
      (let ((kind (car form)))
        (case kind
          (:pt
           (let ((subtype (nth 1 form))
                 (pointer-count (nth 2 form)))
             (and (<= 2 (length form) 3)
                  (or (null pointer-count)
                      (and
                       (integerp pointer-count)
                       (> pointer-count 0)))
                  (type? subtype)
                  kind)))
          (:fn
              (let ((to-type (nth 1 form))
                    (from-types (nth 2 form)))
                (and (= 3 (length form))
                     (type? to-type)
                     (every #'type? from-types)
                     kind)))
          (:struct
           (let ((name (nth 1 form)))
             (and (= 2 (length form))
                  (or (stringp name)
                      (keywordp name))  ; TODO better error message for failure
                  kind)))
          (:array
           (let ((length (nth 1 form))
                 (subtype (nth 2 form)))
             (and (= 3 (length form))
                  (or (null length)
                      (and (> length 0)
                           (integerp length)))
                  (type? subtype)
                  kind)))))))

(assert
 (and
  (type? :int)
  (type? '(:pt (:array nil (:struct :X)) 3))
  (type? '(:pt :int))                   ; by default, this is a 1-pointer
  (type? '(:pt :int 3))                   ; by default, this is a 3-pointer
  (type? '(:array () :int))
  (type? '(:array 1 :int))
  (type? '(:fn :int (:int (:pt (:array () :int) 1))))
  (not (type? '(:pt :int 0)))
  (not (type? '(:array 0 :int)))
  (not (type? '(:array 1 (:array 0 :int))))))

(defun fmt-string<-type (type &optional no-filler)
  (assert (type? type))
  (let ((kind (type? type)))
    (case kind
      (:pt
       (let ((subtype (nth 1 type))
             (pointer-count (nth 2 type)))
         (unless pointer-count
           (setf pointer-count 1))
         (format nil
                 (fmt-string<-type subtype)
                 (if no-filler
                     (format nil "~{~a~}"
                             (make-list pointer-count :initial-element '*))
                     (format nil "~{~a~}(~~a)"
                             (make-list pointer-count :initial-element '*))))))
      (:fn
          (let ((to-type (nth 1 type))
                (from-types (nth 2 type)))
            (format nil
                    (fmt-string<-type to-type)
                    (if no-filler
                        (format nil "(~{~a,~})"
                                (loop :for type :in from-types
                                      :collect (fmt-string<-type type t)))
                        (format nil "(~~a)(~{~a~^,~})"
                                (loop :for type :in from-types
                                      :collect (fmt-string<-type type t)))))))
      (:struct
       (let ((name (nth 1 type)))
         (if no-filler
             (format nil "struct ~a" name)
             (format nil "struct ~a (~~a)" name))))
      (:array
       (let ((length (nth 1 type))
             (subtype (nth 2 type)))
         (unless length (setf length ""))
         (format nil
                 (fmt-string<-type subtype)
                 (if no-filler
                     (format nil "[~a]" length)
                     (format nil "(~~a)[~a]" length)))))
      (t
       (format nil
               (if no-filler
                   "~a"
                   "~a (~~a)")
               (string-downcase (symbol-name kind)))))
    ))

;; https://cdecl.org/
(assert
 (and
  (equal (fmt-string<-type :int)
         "int (~a)")
  (equal (fmt-string<-type '(:array () :int))
         "int ((~a)[])")
  (equal (fmt-string<-type '(:array 9 :int))
         "int ((~a)[9])")
  (equal (fmt-string<-type '(:array 9 (:pt :int 2)))
         "int (**((~a)[9]))")
  (equal (fmt-string<-type '(:pt (:array 9 (:pt :int)) 2))
         "int (*((**(~a))[9]))")

  ;; declare foo as pointer to pointer to pointer to int
  (equal (fmt-string<-type '(:pt :int 3))
         "int (***(~a))")

  ;; declare foo as pointer to int
  (equal (fmt-string<-type '(:pt :int 1))
         "int (*(~a))")

  (equal (fmt-string<-type '(:pt :int))
         "int (*(~a))")

  ;; declare foo as array of pointer to array of int
  ;; int ((*((foo)[]))[])
  (equal
   "int ((*((~a)[]))[])"
   (fmt-string<-type '(:array () (:pt (:array () :int)))))

  ;; declare foo as pointer to array of pointer to array of int
  ;; int (*(*foo)[])[]
  (equal
   "int ((*((*(~a))[]))[])"
   (fmt-string<-type '(:pt (:array () (:pt (:array () :int))))))

  ;; declare foo as pointer to array of int
  ;; int ((*foo)[])
  (equal "int ((*(~a))[])"
         (fmt-string<-type '(:pt (:array () :int))))

  ;; declare foo as array of int
  ;; int ((foo)[])
  (equal "int ((~a)[])"
         (fmt-string<-type '(:array () :int)))

  ;; declare foo as array of array of int
  ;; int (((foo)[])[])
  (equal "int (((~a)[])[])"
         (fmt-string<-type '(:array () (:array () :int))))

  ;; declare foo as pointer to array 9 of struct X
  ;; struct X ((*(foo))[9])
  (equal
   "struct X ((*(~a))[9])"
   (fmt-string<-type '(:pt (:array 9 (:struct :X)))))

  ;; declare foo as struct X
  (equal
   "struct X (~a)"
   (fmt-string<-type '(:struct :X)))

  ;; declare foo as function returning int
  ;; int ((foo)())
  (equal
   "int ((~a)())"
   (fmt-string<-type '(:fn :int ())))

  ;; declare foo as function (int, char) returning int
  ;; int ((foo)(int,char))
  (equal
   "int ((~a)(int,char))"
   (fmt-string<-type '(:fn :int (:int :char))))

  ;; declare foo as function (int, char) returning void
  ;; void ((foo)(int,char))
  (equal
   "void ((~a)(int,char))"
   (fmt-string<-type '(:fn :void (:int :char))))

  ;; declare foo as function (void, struct *X) returning void
  ;; void ((foo)(void,char))
  (equal
   "int ((~a)(int,char))"
   (fmt-string<-type '(:fn :int (:int :char))))

  ;; declare foo as function (void, pointer to struct X) returning void
  ;; void ((foo)(void,struct X (*)))
  (equal
   "void ((~a)(void,struct X (*)))"
   (fmt-string<-type '(:fn :void (:void (:pt (:struct :X))))))

  ;; declare foo as function (int, char, function returning array of void) returning void
  ;; void foo(int , char , void ()[])
  (equal
   "void ((~a)(int,char,void ((())[])))"
   (fmt-string<-type '(:fn :void (:int :char (:fn (:array () :void) ())))))



  (equal
   "void ((~a)[])"
   (fmt-string<-type '(:array () :void)))
  (equal
   "void (((~a)())[])"
   (fmt-string<-type '(:fn (:array () :void) ())))

  ;; declare foo as function returning pointer to array 9 of struct X
  ;; struct X ((*((foo)()))[9])
  (equal
   "struct X ((*((~a)()))[9])"
   (fmt-string<-type '(:fn (:pt (:array 9 (:struct :X))) ())))

  ;; declare foo as pointer to function returning array 9 of struct X
  ;; struct X (((*(foo))())[9])
  (equal
   "struct X (((*(~a))())[9])"
   (fmt-string<-type '(:pt (:fn (:array 9 (:struct :X)) ()))))

  ;; A complicated example (http://unixwiz.net/techtips/reading-cdecl.html)
  ;;
  ;; foo is array of array of 8 pointer to pointer to function returning pointer
  ;; to array of pointer to char
  ;;
  ;; (foo
  ;;  (:array ()
  ;;          (:array 8
  ;;                  (:pt (:pt (:fn ()
  ;;                                 (:pt
  ;;                                  (:array
  ;;                                   ()
  ;;                                   (:pt :char)))))))))
  ;;
  ;; char (* ((* ((* (* ((foo []) [8]))) ())) []))
  ;;
  ;; declare foo as array of array 8 of pointer to pointer to function returning
  ;; pointer to array of pointer to char
  ;;
  ;; char (*((*((*(*(((foo)[])[8])))()))[]))
  (equal
   "char (*((*((*(*(((~a)[])[8])))()))[]))"
   (fmt-string<-type
    '(:array ()
      (:array 8
       (:pt
        (:pt
         (:fn
             (:pt
              (:array
               ()
               (:pt :char)))
             ())))))))

  ;; An example from https://cdecl.org/
  ;;
  ;; declare foo as pointer to function (void) returning pointer to array 3 of int
  ;;
  ;; int ((*((*(foo))(void)))[3])
  (equal
   "int ((*((*(~a))(void)))[3])"
   (fmt-string<-type
    '(:pt (:fn (:pt (:array 3 :int))
              (:void)))))

  ;; TODO Support `volatile pointer`
  ;;
  ;; An example from https://cdecl.org/
  ;;
  ;; declare bar as volatile pointer to array 64 of const int
  ;;
  ;; const int (* volatile bar)[64]

  ;; TODO Support `cast`.
  ;;
  ;; An example from https://cdecl.org/
  ;;
  ;; cast foo into block(int, long long) returning double
  ;;
  ;; (double (^)(int , long long ))foo
  ))
