(in-package :paren)

;; TODO Redo for the type system: Basic types are represented as keywords,
;; while composed types are represented as lists whose cars are of the
;; following: :pointer, :function, :struct, :array.

;; TODO The users can write instead :pointer, :function, :(), :{}, :[]
;; respectively, but a normalizer will transform it into the canonical, longer
;; forms. Write util functions to handle and inspect types. Allow users to
;; define more type operators (e.g. :pointer :function as above).

;; TODO Finally, write a printer, and integrate that with SET and DECLARE.
;; Mention that the type system is by no means complete, yet the user can
;; inline any C codes so that's not a problem.

;; TODO Change syntax from (:array () :int) to (:array :int)

;; TODO Support `volatile pointer`. (an example from https://cdecl.org/):
;; declare bar as volatile pointer to array 64 of const int
;;
;; const int (* volatile bar)[64]

;; TODO Support `cast`. An example from https://cdecl.org/: cast foo into
;; block(int, long long) returning double
;;
;; (double (^)(int , long long ))foo

(defun type? (form)
  (if (keywordp form)
      form
      (let ((kind (car form)))
        (case kind
          (:pointer
           (let ((subtype (nth 1 form))
                 (pointer-count (nth 2 form)))
             (and (<= 2 (length form) 3)
                  (or (null pointer-count)
                      (and
                       (integerp pointer-count)
                       (> pointer-count 0)))
                  (type? subtype)
                  kind)))
          (:function
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
                      (keywordp name)) ; TODO better error message for failure
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
  (type? '(:pointer (:array nil (:struct :X)) 3))
  (type? '(:pointer :int))                   ; by default, this is a 1-pointer
  (type? '(:pointer :int 3))                   ; by default, this is a 3-pointer
  (type? '(:array () :int))
  (type? '(:array 1 :int))
  (type? '(:function :int (:int (:pointer (:array () :int) 1))))
  (not (type? '(:pointer :int 0)))
  (not (type? '(:array 0 :int)))
  (not (type? '(:array 1 (:array 0 :int))))))

(defun fmt-string<-type (type &optional no-filler)
  (assert (type? type))
  (let ((kind (type? type)))
    (case kind
      (:pointer
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
      (:function
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
  (equal (fmt-string<-type '(:array 9 (:pointer :int 2)))
         "int (**((~a)[9]))")
  (equal (fmt-string<-type '(:pointer (:array 9 (:pointer :int)) 2))
         "int (*((**(~a))[9]))")

  ;; declare foo as pointer to pointer to pointer to int
  (equal (fmt-string<-type '(:pointer :int 3))
         "int (***(~a))")

  ;; declare foo as pointer to int
  (equal (fmt-string<-type '(:pointer :int 1))
         "int (*(~a))")

  (equal (fmt-string<-type '(:pointer :int))
         "int (*(~a))")

  ;; declare foo as array of pointer to array of int
  ;; int ((*((foo)[]))[])
  (equal
   "int ((*((~a)[]))[])"
   (fmt-string<-type '(:array () (:pointer (:array () :int)))))

  ;; declare foo as pointer to array of pointer to array of int
  ;; int (*(*foo)[])[]
  (equal
   "int ((*((*(~a))[]))[])"
   (fmt-string<-type '(:pointer (:array () (:pointer (:array () :int))))))

  ;; declare foo as pointer to array of int
  ;; int ((*foo)[])
  (equal "int ((*(~a))[])"
         (fmt-string<-type '(:pointer (:array () :int))))

  ;; declare foo as array of int
  ;; int ((foo)[])
  (equal "int ((~a)[])"
         (fmt-string<-type '(:array () :int)))

  ;; declare foo as array of array of int
  ;; int (((foo)[])[])
  (equal "int (((~a)[])[])"
         (fmt-string<-type '(:array () (:array () :int))))
  ;; TODO Contribution Opportunity - Can we improve the printer to reduce
  ;; unnecessary parentheses? For example, the output should ideally be "int
  ;; foo[][]" instead of "int (((foo)[])[])". Contributions with a proof or
  ;; supporting evidence are welcome.

  ;; declare foo as pointer to array 9 of struct X
  ;; struct X ((*(foo))[9])
  (equal
   "struct X ((*(~a))[9])"
   (fmt-string<-type '(:pointer (:array 9 (:struct :X)))))

  ;; declare foo as struct X
  (equal
   "struct X (~a)"
   (fmt-string<-type '(:struct :X)))

  ;; declare foo as function returning int
  ;; int ((foo)())
  (equal
   "int ((~a)())"
   (fmt-string<-type '(:function :int ())))

  ;; declare foo as function (int, char) returning int
  ;; int ((foo)(int,char))
  (equal
   "int ((~a)(int,char))"
   (fmt-string<-type '(:function :int (:int :char))))

  ;; declare foo as function (int, char) returning void
  ;; void ((foo)(int,char))
  (equal
   "void ((~a)(int,char))"
   (fmt-string<-type '(:function :void (:int :char))))

  ;; declare foo as function (void, struct *X) returning void
  ;; void ((foo)(void,char))
  (equal
   "int ((~a)(int,char))"
   (fmt-string<-type '(:function :int (:int :char))))

  ;; declare foo as function (void, pointer to struct X) returning void
  ;; void ((foo)(void,struct X (*)))
  (equal
   "void ((~a)(void,struct X (*)))"
   (fmt-string<-type '(:function :void (:void (:pointer (:struct :X))))))

  ;; declare foo as function (int, char, function returning array of void) returning void
  ;; void foo(int , char , void ()[])
  (equal
   "void ((~a)(int,char,void ((())[])))"
   (fmt-string<-type '(:function :void (:int :char (:function (:array () :void) ())))))



  (equal
   "void ((~a)[])"
   (fmt-string<-type '(:array () :void)))
  (equal
   "void (((~a)())[])"
   (fmt-string<-type '(:function (:array () :void) ())))

  ;; declare foo as function returning pointer to array 9 of struct X
  ;; struct X ((*((foo)()))[9])
  (equal
   "struct X ((*((~a)()))[9])"
   (fmt-string<-type '(:function (:pointer (:array 9 (:struct :X))) ())))

  ;; declare foo as pointer to function returning array 9 of struct X
  ;; struct X (((*(foo))())[9])
  (equal
   "struct X (((*(~a))())[9])"
   (fmt-string<-type '(:pointer (:function (:array 9 (:struct :X)) ()))))

  ;; A complicated example (http://unixwiz.net/techtips/reading-cdecl.html)
  ;;
  ;; foo is array of array of 8 pointer to pointer to function returning pointer
  ;; to array of pointer to char
  ;;
  ;; (foo
  ;;  (:array ()
  ;;          (:array 8
  ;;                  (:pointer (:pointer (:function ()
  ;;                                 (:pointer
  ;;                                  (:array
  ;;                                   ()
  ;;                                   (:pointer :char)))))))))
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
       (:pointer
        (:pointer
         (:function
             (:pointer
              (:array
               ()
               (:pointer :char)))
             ())))))))

  ;; An example from https://cdecl.org/
  ;;
  ;; declare foo as pointer to function (void) returning pointer to array 3 of int
  ;;
  ;; int ((*((*(foo))(void)))[3])
  (equal
   "int ((*((*(~a))(void)))[3])"
   (fmt-string<-type
    '(:pointer (:function (:pointer (:array 3 :int))
              (:void)))))))
