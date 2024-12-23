(in-package :paren.test)

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

  ;; declare foo as pointer to pointer to pointer to int
  (is
   (equal (paren::fmt-string<-type '(:pointer :int 3))
          "int (***(~a))"))

  ;; declare foo as pointer to int
  (is
   (equal (paren::fmt-string<-type '(:pointer :int 1))
          "int (*(~a))"))
 
  (is
   (equal (paren::fmt-string<-type '(:pointer :int))
          "int (*(~a))"))

  ;; declare foo as array of pointer to array of int
  ;;
  ;; int ((*((foo)[]))[])
  (is
   (equal
    "int ((*((~a)[]))[])"
    (paren::fmt-string<-type '(:array () (:pointer (:array () :int))))))

  ;; declare foo as pointer to array of pointer to array of int
  ;;
  ;; int (*(*foo)[])[]
  (is
   (equal
    "int ((*((*(~a))[]))[])"
    (paren::fmt-string<-type '(:pointer (:array () (:pointer (:array () :int)))))))

  ;; declare foo as pointer to array of int
  ;;
  ;; int ((*foo)[])
  (is
   (equal "int ((*(~a))[])"
          (paren::fmt-string<-type '(:pointer (:array () :int)))))

  ;; declare foo as array of int
  ;;
  ;; int ((foo)[])
  (is
   (equal "int ((~a)[])"
          (paren::fmt-string<-type '(:array () :int))))

  ;; declare foo as array of array of int
  ;;
  ;; int (((foo)[])[])
  (is
   (equal "int (((~a)[])[])"
          (paren::fmt-string<-type '(:array () (:array () :int)))))

  ;; TODO Contribution Opportunity - Can we improve the printer to reduce
  ;; unnecessary parentheses? For example, the output should ideally be "int
  ;; foo[][]" instead of "int (((foo)[])[])". Contributions with a proof or
  ;; supporting evidence are welcome.
  ;;
  ;; declare foo as pointer to array 9 of struct X
  ;;
  ;; struct X ((*(foo))[9])
  (is
   (equal
    "struct x ((*(~a))[9])"
    (paren::fmt-string<-type '(:pointer (:array 9 (:struct :x))))))

  ;; declare foo as struct X
  (is
   (equal
    "struct x (~a)"
    (paren::fmt-string<-type '(:struct :x))))

  ;; declare foo as function returning int
  ;;
  ;; int ((foo)())
  (is
   (equal
    "int ((~a)())"
    (paren::fmt-string<-type '(:function :int ()))))

  ;; declare foo as function (int, char) returning int
  ;;
  ;; int ((foo)(int,char))
  (is
   (equal
    "int ((~a)(int,char))"
    (paren::fmt-string<-type '(:function :int (:int :char)))))

  ;; declare foo as function (int, char) returning void
  ;;
  ;; void ((foo)(int,char))
  (is
   (equal
    "void ((~a)(int,char))"
    (paren::fmt-string<-type '(:function :void (:int :char)))))

  ;; declare foo as function (void, struct *X) returning void
  ;;
  ;; void ((foo)(void,char))
  (is
   (equal
    "int ((~a)(int,char))"
    (paren::fmt-string<-type '(:function :int (:int :char)))))

  ;; declare foo as function (void, pointer to struct X) returning void
  ;;
  ;; void ((foo)(void,struct X (*)))
  (is
   (equal
    "void ((~a)(void,struct x (*)))"
    (paren::fmt-string<-type '(:function :void (:void (:pointer (:struct :x)))))))

  ;; declare foo as function (int, char, function returning array of void) returning void
  ;;
  ;; void foo(int , char , void ()[])
  (is
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

  ;; declare foo as function returning pointer to array 9 of struct X
  ;;
  ;; struct X ((*((foo)()))[9])
  (is
   (equal
    "struct x ((*((~a)()))[9])"
    (paren::fmt-string<-type '(:function (:pointer (:array 9 (:struct :x))) ()))))

  ;; declare foo as pointer to function returning array 9 of struct X
  ;;
  ;; struct X (((*(foo))())[9])
  (is
   (equal
    "struct x (((*(~a))())[9])"
    (paren::fmt-string<-type '(:pointer (:function (:array 9 (:struct :x)) ())))))

  ;; A complicated example (http://unixwiz.net/techtips/reading-cdecl.html)
  ;;
  ;; foo is array of array of 8 pointer to pointer to function returning
  ;; pointer to array of pointer to char
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
  ;; declare foo as array of array 8 of pointer to pointer to function
  ;; returning pointer to array of pointer to char
  ;;
  ;; char (*((*((*(*(((foo)[])[8])))()))[]))
  (is
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
  (is (string= "" (paren::invert-case "")))
  (is (string= "hello" (paren::invert-case "HELLO")))
  (is (string= "我hello" (paren::invert-case "我HELLO")))
  (is (string= "HELLO" (paren::invert-case "hello")))
  (is (string= "我HELLO" (paren::invert-case "我hello")))
  (is (string= "HelloWorld" (paren::invert-case "HelloWorld")))
  (is (string= "我HelloWorld" (paren::invert-case "我HelloWorld"))))

(test resolve-declaration               ; util
  (is (equal "int (x)"
             (paren::resolve-declaration '(x :int))))
  (is (equal "int (x_y)"
             (paren::resolve-declaration '(x-y :int))))
  (is (equal "int (*(x))"
             (paren::resolve-declaration '(x (:pointer :int 1)))))
  (is (equal "char (**(argv))"
             (paren::resolve-declaration '(argv (:pointer :char 2)))))
  (is (equal "x (*(next))"
             (paren::resolve-declaration '(next (:pointer :x)))))
  (is (equal "struct x (window)"
             (paren::resolve-declaration '(window (:struct :x)))))
  (is (equal "struct X (window)"
             (paren::resolve-declaration '(window (:struct :|x|)))))
  (is (equal "struct xX (window)"
             (paren::resolve-declaration '(window (:struct :|xX|))))))

(test symbols
  (is (equal "abcde" (c 'abcde)))
  (is (equal "abCde" (c '|abCde|)))
  (is (equal "ABCDE" (c '|abcde|))))

(test deftype
  (is (equal "typedef struct x x;"
             (c `(deftype (:struct :x) x))))
  (is (equal "typedef struct y y;"
             (c `(deftype (:struct :y) y))))
  (is (equal "typedef struct Y y;"
             (c `(deftype (:struct :|y|) y)))))

(test defstruct
  (is (equal
       (format nil "~:
struct x {
  int (value);
  x (*(next));
};")
       (c `(defstruct x
             ((value :int)
              (next (:pointer :x)))))
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
             (c `(vec 1 2 3))))
  (is (equal
       "int ((x)[3]) = {1, 2, 3}"
       ;; TODO Maybe vec should just be an alias to array.
       (c '(set (x (:array 3 :int)) (vec 1 2 3))))))

(test ->
  (is (equal
       (c `(-> x1 value))
       "x1->value"))
  (is (equal
       "x1->next->next->value"
       (c `(-> x1 next next value))))
  (is (equal
       "printf(\"Answer: %d.\\n\", x1->next->next->value)"
       (c `(@printf (str "Answer: %d.\\n")
                    (-> x1 next next value))))))

(test string
  (is (equal
       "\"ABCd\""
       (c '(str "ABCd"))))
  (is (equal
       "\"ABCD\""
       (c '(str "ABCD"))))
  (is (equal
       "\"DEFUN\""
       (c '(str "DEFUN")))))

(test ==
  (is (equal
       "(i == 0)"
       (c '(== i 0)))))

(test or
  (is (equal
       "((i == 15) || (i == 20))"
       (c `(or (== i 15)
               (== i 20))))))

(test and
  (is (equal
       "((i > 0) && (i < 30))"
       (c `(and (> i 0)
                (< i 30))))))

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
             (return 0)))))

  (is (equal
       (format nil "~:
int (main) (int (argc), char (**(argv))) {
  return 0;
}")
       (c `(defun (main :int) ((argc :int) (argv (:pointer :char 2)))
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

  ;; NOTE Handy for users who just want to insert some code without learning
  ;; this language paren.
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

(test integration-test?
  (is
   (string=
    (c `(@scanf (str "%d") (& day)))
    "scanf(\"%d\", &day)"))

  (is
   (string=
    (c `(case day
          (1 (return 1))
          (2 (return 2))
          (t (return 0))))
    (format nil
            "~:
switch (day) {
  case 1:
    return 1;
  case 2:
    return 2;
  default:
    return 0;
}"))))
