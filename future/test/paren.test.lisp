(in-package :paren.test)

(setf (readtable-case *readtable*) :invert)

(def-suite paren.test)
(in-suite paren.test)

;;; Tests

(test type?
  (is (paren::type? :int))
  (is (paren::type? '(:pointer (:array nil (:struct :X)) 3)))
  (is (paren::type? '(:pointer :int)))        ; by default, this is a 1-pointer
  (is (paren::type? '(:pointer :int 3)))      ; by default, this is a 3-pointer
  (is (paren::type? '(:array () :int)))
  (is (paren::type? '(:array 1 :int)))
  (is (paren::type? '(:function :int (:int (:pointer (:array () :int) 1)))))
  (is (not (paren::type? '(:pointer :int 0))))
  (is (not (paren::type? '(:array 0 :int))))
  (is (not (paren::type? '(:array 1 (:array 0 :int))))))

(test fmt-string<-type
  ;; NOTE https://cdecl.org/
  (is (equal (paren::fmt-string<-type :int)
             "int (~a)"))
  (is (equal (paren::fmt-string<-type '(:array () :int))
             "int ((~a)[])"))
  (is (equal (paren::fmt-string<-type '(:array 9 :int))
             "int ((~a)[9])"))
  (is (equal (paren::fmt-string<-type '(:array 9 (:pointer :int 2)))
             "int (**((~a)[9]))"))
  (is (equal (paren::fmt-string<-type '(:pointer (:array 9 (:pointer :int)) 2))
             "int (*((**(~a))[9]))"))
  (is
   ;; declare foo as pointer to pointer to pointer to int
   (equal (paren::fmt-string<-type '(:pointer :int 3))
          "int (***(~a))"))
  (is
   ;; declare foo as pointer to int
   (equal (paren::fmt-string<-type '(:pointer :int 1))
          "int (*(~a))"))
  (is
   (equal (paren::fmt-string<-type '(:pointer :int))
          "int (*(~a))"))
  (is
   ;; declare foo as array of pointer to array of int
   ;; int ((*((foo)[]))[])
   (equal
    "int ((*((~a)[]))[])"
    (paren::fmt-string<-type '(:array () (:pointer (:array () :int))))))
  (is
   ;; declare foo as pointer to array of pointer to array of int
   ;; int (*(*foo)[])[]
   (equal
    "int ((*((*(~a))[]))[])"
    (paren::fmt-string<-type '(:pointer (:array () (:pointer (:array () :int)))))))
  (is
   ;; declare foo as pointer to array of int
   ;; int ((*foo)[])
   (equal "int ((*(~a))[])"
          (paren::fmt-string<-type '(:pointer (:array () :int)))))
  (is
   ;; declare foo as array of int
   ;; int ((foo)[])
   (equal "int ((~a)[])"
          (paren::fmt-string<-type '(:array () :int))))
  (is
   ;; declare foo as array of array of int
   ;; int (((foo)[])[])
   (equal "int (((~a)[])[])"
          (paren::fmt-string<-type '(:array () (:array () :int)))))
  (is ;; TODO Contribution Opportunity - Can we improve the printer to reduce
   ;; unnecessary parentheses? For example, the output should ideally be "int
   ;; foo[][]" instead of "int (((foo)[])[])". Contributions with a proof or
   ;; supporting evidence are welcome.

   ;; declare foo as pointer to array 9 of struct X
   ;; struct X ((*(foo))[9])
   (equal
    "struct X ((*(~a))[9])"
    (paren::fmt-string<-type '(:pointer (:array 9 (:struct :X))))))
  (is
   ;; declare foo as struct X
   (equal
    "struct X (~a)"
    (paren::fmt-string<-type '(:struct :X))))
  (is
   ;; declare foo as function returning int
   ;; int ((foo)())
   (equal
    "int ((~a)())"
    (paren::fmt-string<-type '(:function :int ()))))
  (is
   ;; declare foo as function (int, char) returning int
   ;; int ((foo)(int,char))
   (equal
    "int ((~a)(int,char))"
    (paren::fmt-string<-type '(:function :int (:int :char)))))
  (is
   ;; declare foo as function (int, char) returning void
   ;; void ((foo)(int,char))
   (equal
    "void ((~a)(int,char))"
    (paren::fmt-string<-type '(:function :void (:int :char)))))
  (is
   ;; declare foo as function (void, struct *X) returning void
   ;; void ((foo)(void,char))
   (equal
    "int ((~a)(int,char))"
    (paren::fmt-string<-type '(:function :int (:int :char)))))
  (is
   ;; declare foo as function (void, pointer to struct X) returning void
   ;; void ((foo)(void,struct X (*)))
   (equal
    "void ((~a)(void,struct X (*)))"
    (paren::fmt-string<-type '(:function :void (:void (:pointer (:struct :X)))))))
  (is
   ;; declare foo as function (int, char, function returning array of void) returning void
   ;; void foo(int , char , void ()[])
   (equal
    "void ((~a)(int,char,void ((())[])))"
    (paren::fmt-string<-type '(:function :void (:int :char (:function (:array () :void) ()))))))
  (is


   (equal
    "void ((~a)[])"
    (paren::fmt-string<-type '(:array () :void))))
  (is (equal
       "void (((~a)())[])"
       (paren::fmt-string<-type '(:function (:array () :void) ()))))
  (is
   ;; declare foo as function returning pointer to array 9 of struct X
   ;; struct X ((*((foo)()))[9])
   (equal
    "struct X ((*((~a)()))[9])"
    (paren::fmt-string<-type '(:function (:pointer (:array 9 (:struct :X))) ()))))
  (is
   ;; declare foo as pointer to function returning array 9 of struct X
   ;; struct X (((*(foo))())[9])
   (equal
    "struct X (((*(~a))())[9])"
    (paren::fmt-string<-type '(:pointer (:function (:array 9 (:struct :X)) ())))))
  (is
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
    (paren::fmt-string<-type
     '(:array ()
       (:array 8
        (:pointer
         (:pointer
          (:function
           (:pointer
            (:array
             ()
             (:pointer :char)))
           ()))))))))

  ;; An example from https://cdecl.org/
  ;;
  ;; declare foo as pointer to function (void) returning pointer to array 3 of int
  ;;
  ;; int ((*((*(foo))(void)))[3])
  (is (equal
       "int ((*((*(~a))(void)))[3])"
       (paren::fmt-string<-type
        '(:pointer (:function (:pointer (:array 3 :int))
                    (:void)))))))

(test invert-case                       ; util
  (is (string= "aBc"  (paren::invert-case "AbC")))
  (is (string= "abc"  (paren::invert-case "ABC")))
  (is (string= "aB1"  (paren::invert-case "Ab1")))
  (is (string= "ABCD" (paren::invert-case "abcd")))
  ;; FIXME Why is this wrong? Very weird bug.. It is correct while manually
  ;; evaluated in the buffer. And it seems to fail only for the string "abc".
  ;;
  ;; (is (string= "ABC" (paren::invert-case "abc")))
  )

(test resolve-declaration               ; util
  (is (equal "int (x)"
             (paren::resolve-declaration '(x :int))))
  (is (equal "int (x_y)"
             (paren::resolve-declaration '(x-y :int))))
  (is (equal "int (X)"
             (paren::resolve-declaration '(X :int))))
  (is (equal "int (*(X))"
             (paren::resolve-declaration '(X (:pointer :int 1)))))
  (is (equal "char (**(argv))"
             (paren::resolve-declaration '(argv (:pointer :char 2)))))
  (is (equal "x (*(next))"
             (paren::resolve-declaration '(next (:pointer :X))))) ; FIXME The printed x should be capitalized.
  (is (equal "struct X (window)"
             (paren::resolve-declaration '(window (:struct :X)))))
  (is (equal "struct X (***(a_b_c))"
             (paren::resolve-declaration '(a-b-c (:pointer (:struct :X) 3)))))
  ;; NOTE: By default, we set the reader case to :INVERT, allowing the user to
  ;; write Lisp code in lowercase. In paren.lisp, we revert the case back.
  ;; However,:INVERT only affects symbols whose unescaped characters are
  ;; uniformly cased. As a result, symbols with mixed-case characters are
  ;; inverted differently.
  (is (equal "int (A_b_C_d_E)"
             (paren::resolve-declaration '(a-B-c-D-e :int))))
  (is (equal "int (AbCdE1)"
             (paren::resolve-declaration '(aBcDe1 :int)))))

;; FIXME Weird bug.. Does altering the reader-case change how a string is read too?
;;
(test symbols
  (is (equal "abcde" (c 'abcde)))
  ;; (is (equal "abcd" (c 'abcd))) ; FIXME
  (is (equal "ABCD" (c 'ABCD)))
  ;; (is (equal "AbC" (c 'aBc))) ; FIXME
  )

(test deftype
  (is (equal "typedef struct x x;"
             (c `(deftype (:struct :x) x))))
  (is (equal "typedef struct Y Y;"
             (c `(deftype (:struct :Y) Y)))))

(test defstruct
  (is (equal
       (format nil "~:
struct X {
  int (value);
  x (*(next));
};")                                    ; FIXME The second x should be capitalized.
       (c `(defstruct X
             ((value :int)
              (next (:pointer :X)))))
       )))

(test funcall
  (is (equal
       "printf(\"x = %d, y = %d \\n\", x, y)"
       (c `(@printf (str "x = %d, y = %d \\n") x y))))
  (is (equal
       "malloc(sizeof(x))"
       (c `(@malloc (@sizeof x))))))

(test array
  (is (equal "{1, 2, 3}"
             (c `(vec 1 2 3)))))

(test ->
  (is (equal
       (c `(-> x1 value))
       "x1->value"))
  (is (equal
       "x1->next->next->value"
       (c `(-> x1 next next value))))
  (is (equal
       "printf(\"Answer: %d.\\n\", x1->next->next->value)"
       (c `(@printf (str "Answer: %d.\\n") (-> x1 next next value))))))

(test string
  (is (equal
       "\"ABCd\""
       (c '(str "ABCd"))))
  (is (equal
       "\"ABCD\""
       (c '(str "ABCD"))))
  (is (equal
       "\"DEFUN\""
       (c '(str "DEFUN"))))
  ;; ;; FIXME Why is this wrong? Very weird bug..
  ;; (is (equal
  ;;      "\"ABC\""
  ;;      (c '(str "ABC"))))
  )

(test ==
  (is (equal
       "(I == 0)"
       (c '(== I 0)))))

(test or
  (is (equal
       "((I == 15) || (I == 20))"
       (c `(or (== I 15)
               (== I 20))))))

(test and
  (is (equal
       "((I > 0) && (I < 30))"
       (c `(and (> I 0)
                (< I 30))))))

(test include
  (is (equal
       (format nil "~:
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include \"emacs.h\"
")
       (c `(include :system ("stdio.h" "stdlib.h" "math.h")
                    :local ("emacs.h")))
       )))

(test type
  (is (equal
       "int (x)"
       (c '(set (x :int)))))
  (is (equal
       "int ((*(x))[])"
       (c '(set (x (:pointer (:array nil :int) 1))))))
  (is (equal
       "int ((x)[])"
       (c '(set (x (:array nil :int))))))
  (is (equal
       "int ((x)[3])"
       (c '(set (x (:array 3 :int))))))
  (is (equal
       "int ((x)[3]) = {1, 2, 3}"
       (c '(set (x (:array 3 :int)) (vec 1 2 3))))) ; TODO Maybe vec should just be an alias to array.
  (is (equal
       "int (*(x))"
       (c '(set (x (:pointer :int 1))))))
  (is (equal
       "int (**(x))"
       (c '(set (x (:pointer :int 2))))))
  (is (equal
       (format nil "~:
int (main) (int (argc), char (**(argv))) {
  return 0;
}")
       (c `(defun (main :int) ((argc :int) (argv (:pointer :char 2)))
             (return 0))))))

(test set
  (is (equal
       "int (x)"
       (c '(set (x :int)))))
  (is (equal
       "int (x) = {1, 2, 3}"
       (c '(set (x :int) (vec 1 2 3)))))
  (is (equal
       "int (*(x))"
       (c '(set (x (:pointer :int 1))))))
  (is (equal
       "int (*(x)) = 42"
       (c '(set (x (:pointer :int 1)) 42)))))

(test defun
  (is (equal
       (format nil "~:
int (main) () {
  int (x) = 5;
  int (y) = 10;
  return 0;
}")
       (c `(defun (main :int) ()
             (set (x :int) 5)
             (set (y :int) 10)
             (return 0))))))

(test assignment
  (is (equal
       "int (x) = 5"
       (c `(set (x :int) 5))))
  (is (equal
       "int (y) = 10"
       (c `(set (y :int) 10)))))

(test return
  (is (equal
       "return 0"
       (c `(return 0)))))

(test inline
  (is (equal
       "42"
       (c 42)))
  (is (equal
       "42.0"
       (c 42.00)))
  (is (equal
       "any code; anywhere;!!"
       (c "any code; anywhere;!!")))
  (is (equal
       (format nil "~:
int (main) () {
  int (*(*foo)(void ))[3];
  const int (* volatile bar)[64];
  (double (^)(int , long long ))baz;
  return 0;
}")
       (c `(defun (main :int) ()
             "int (*(*foo)(void ))[3]"
             "const int (* volatile bar)[64]"
             "(double (^)(int , long long ))baz"
             (return 0))))))
