(in-package :paren.test)

(def-suite paren.test)
(in-suite paren.test)

;;; Tests

(test type?
  (is (paren::type? :int))
  (is (paren::type? '(:pointer (:array nil (:struct :X)) 3)))
  (is (paren::type? '(:pointer :int)))   ; by default, this is a 1-pointer
  (is (paren::type? '(:pointer :int 3))) ; by default, this is a 3-pointer
  (is (paren::type? '(:array () :int)))
  (is (paren::type? '(:array 1 :int)))
  (is (paren::type? '(:array (octal-int 10000) :int)))
  (is (paren::type? '(:function :int (:int (:pointer (:array () :int) 1)))))
  (is (not (paren::type? '(:pointer :int 0))))
  ;; (is (not (paren::type? '(:array 0 :int))))
  ;; (is (not (paren::type? '(:array 1 (:array 0 :int)))))
  )

(test fmt-string<-type
  ;; NOTE https://cdecl.org/
  (is (equal (paren::fmt-string<-type :int) "int ~a"))
  (is (equal (paren::fmt-string<-type :short) "short ~a"))
  (is (equal (paren::fmt-string<-type :long) "long ~a"))
  (is (equal (paren::fmt-string<-type :long2) "long long ~a"))
  (is (equal (paren::fmt-string<-type :uint) "unsigned int ~a"))
  (is (equal (paren::fmt-string<-type :ushort) "unsigned short ~a"))
  (is (equal (paren::fmt-string<-type :ulong) "unsigned long ~a"))
  (is (equal (paren::fmt-string<-type :ulong2) "unsigned long long ~a"))
  (is (equal (paren::fmt-string<-type :char) "char ~a"))
  (is (equal (paren::fmt-string<-type :schar) "signed char ~a"))
  (is (equal (paren::fmt-string<-type :uchar) "unsigned char ~a"))
  (is (equal (paren::fmt-string<-type :float) "float ~a"))
  (is (equal (paren::fmt-string<-type :double) "double ~a"))
  (is (equal (paren::fmt-string<-type :ldouble) "long double ~a"))
  (is (equal (paren::fmt-string<-type :size-t) "size_t ~a"))

  (is (equal (paren::fmt-string<-type '(:array () :int))
             "int (~a)[]"))
  (is (equal (paren::fmt-string<-type '(:array 9 :int))
             "int (~a)[9]"))
  (is (equal (paren::fmt-string<-type '(:array (octal-int 100000) :int))
             "int (~a)[0100000]"))
  (is (equal (paren::fmt-string<-type '(:array 9 (:pointer :int 2)))
             "int **((~a)[9])"))
  (is (equal (paren::fmt-string<-type '(:pointer (:array 9 (:pointer :int)) 2))
             "int *((**(~a))[9])"))

  ;; declare foo as pointer to pointer to pointer to int
  (is
   (equal (paren::fmt-string<-type '(:pointer :int 3))
          "int ***(~a)"))

  ;; declare foo as pointer to int
  (is
   (equal (paren::fmt-string<-type '(:pointer :int 1))
          "int *(~a)"))
 
  (is
   (equal (paren::fmt-string<-type '(:pointer :int))
          "int *(~a)"))

  ;; declare foo as array of pointer to array of int
  ;;
  ;; int ((*((foo)[]))[])
  (is
   (equal
    "int (*((~a)[]))[]"
    (paren::fmt-string<-type '(:array () (:pointer (:array () :int))))))

  ;; declare foo as pointer to array of pointer to array of int
  ;;
  ;; int (*(*foo)[])[]
  (is
   (equal
    "int (*((*(~a))[]))[]"
    (paren::fmt-string<-type '(:pointer (:array () (:pointer (:array () :int)))))))

  ;; declare foo as pointer to array of int
  ;;
  ;; int ((*foo)[])
  (is
   (equal "int (*(~a))[]"
          (paren::fmt-string<-type '(:pointer (:array () :int)))))

  ;; declare foo as array of int
  ;;
  ;; int ((foo)[])
  (is
   (equal "int (~a)[]"
          (paren::fmt-string<-type '(:array () :int))))

  ;; declare foo as array of array of int
  ;;
  ;; int (((foo)[])[])
  (is
   (equal "int ((~a)[])[]"
          (paren::fmt-string<-type '(:array () (:array () :int)))))

  ;; declare foo as pointer to array 9 of struct X
  ;;
  ;; struct X ((*(foo))[9])
  (is
   (equal
    "struct x (*(~a))[9]"
    (paren::fmt-string<-type '(:pointer (:array 9 (:struct :x))))))

  ;; declare foo as struct X
  (is
   (equal
    "struct x ~a"
    (paren::fmt-string<-type '(:struct :x))))

  ;; declare foo as function returning int
  ;;
  ;; int ((foo)())
  (is
   (equal
    "int (~a) ()"
    (paren::fmt-string<-type '(:function :int ()))))

  ;; declare foo as function (int, char) returning int
  ;;
  ;; int ((foo)(int,char))
  (is
   (equal
    "int (~a) (int,char)"
    (paren::fmt-string<-type '(:function :int (:int :char)))))

  ;; declare foo as function (int, char) returning void
  ;;
  ;; void ((foo)(int,char))
  (is
   (equal
    "void (~a) (int,char)"
    (paren::fmt-string<-type '(:function :void (:int :char)))))

  ;; declare foo as function (void, struct *X) returning void
  ;;
  ;; void ((foo)(void,char))
  (is
   (equal
    "int (~a) (int,char)"
    (paren::fmt-string<-type '(:function :int (:int :char)))))

  ;; declare foo as function (void, pointer to struct X) returning void
  ;;
  ;; void ((foo)(void,struct X (*)))
  (is
   (equal
    "void (~a) (void,struct x *)"
    (paren::fmt-string<-type '(:function :void (:void (:pointer (:struct :x)))))))

  ;; declare foo as function (int, char, function returning array of void) returning void
  ;;
  ;; void foo(int , char , void ()[])
  (is
   (equal
    "void (~a) (int,char,void (())[])"
    (paren::fmt-string<-type '(:function :void (:int :char (:function (:array () :void) ()))))))

  (is
   (equal
    "void (~a)[]"
    (paren::fmt-string<-type '(:array () :void))))

  (is (equal
       "void ((~a) ())[]"
       (paren::fmt-string<-type '(:function (:array () :void) ()))))

  ;; declare foo as function returning pointer to array 9 of struct x
  ;;
  ;; struct X ((*((foo)()))[9])
  (is
   (equal
    "struct x (*((~a) ()))[9]"
    (paren::fmt-string<-type '(:function (:pointer (:array 9 (:struct :x))) ()))))

  ;; declare foo as pointer to function returning array 9 of struct x
  ;;
  ;; struct X (((*(foo))())[9])
  (is
   (equal
    "struct x ((*(~a)) ())[9]"
    (paren::fmt-string<-type '(:pointer (:function (:array 9 (:struct :x)) ())))))

  ;; A complicated example (http://unixwiz.net/techtips/reading-cdecl.html)
  ;;
  ;; declare foo as array of array 8 of pointer to pointer to function
  ;; returning pointer to array of pointer to char
  ;;
  ;; char (*((*((*(*(((foo)[])[8])))()))[]))
  (is
   (equal
    "char *((*((*(*(((~a)[])[8]))) ()))[])"
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

  ;; A variation of the example above.
  ;;
  ;; char *((*(*(*(((foo)[])[8])) ()))[])
  ;;
  ;; declare foo as array of array 8 of pointer to function returning pointer
  ;; to pointer to array of pointer to char
  (is
   (equal
    "char *((*(*((*(((~a)[])[8])) ())))[])"
    (paren::fmt-string<-type
     '(:array ()
       (:array 8
        (:pointer
         (:function
          (:pointer
           (:pointer
            (:array
             ()
             (:pointer :char))))
          ())))))))

  ;; An example from https://cdecl.org/
  ;;
  ;; declare foo as pointer to function (void) returning pointer to array 3 of int
  ;;
  ;; int ((*((*(foo))(void)))[3])
  (is (equal
       "int (*((*(~a)) (void)))[3]"
       (paren::fmt-string<-type
        '(:pointer (:function (:pointer (:array 3 :int))
                    (:void))))))

  ;; declare foo as const int
  ;; const int foo
  (is (equal "const int ~a"
             (paren::fmt-string<-type :c-int)))

  ;; declare foo as volatile int
  ;; volatile int foo
  (is (equal "volatile int ~a"
             (paren::fmt-string<-type :v-int)))

  ;; declare foo as const volatile int
  ;; const volatile int foo
  (is (equal "const volatile int ~a"
             (paren::fmt-string<-type :cv-int)))

  ;; declare foo as volatile pointer to const volatile int
  ;; const volatile int (* volatile foo)
  (is (equal "const volatile int *volatile (~a)"
             (paren::fmt-string<-type '(:v-pointer :cv-int))))

  ;; declare foo as volatile pointer to pointer to const volatile int
  ;; const volatile int ** volatile foo
  (is (equal "const volatile int *(*volatile (~a))"
             (paren::fmt-string<-type '(:v-pointer (:pointer :cv-int)))))

  ;; declare foo as volatile pointer to volatile pointer to const volatile int
  ;; const volatile int (* volatile (* volatile foo))
  (is (equal "const volatile int *volatile *volatile (~a)"
             (paren::fmt-string<-type '(:v-pointer :cv-int 2))))

  ;; declare foo as function returning volatile pointer to volatile pointer to const volatile int
  ;; const volatile int (* volatile (* volatile (foo())))
  (is (equal "const volatile int *volatile *volatile ((~a) ())"
             (paren::fmt-string<-type '(:function (:v-pointer :cv-int 2) ()))))

  ;; declare foo as volatile pointer to array 64 of const int
  ;; const int ((* volatile foo)[64])
  (is (equal "const int (*volatile (~a))[64]"
             (paren::fmt-string<-type '(:v-pointer (:array 64 :c-int)))))

  ;; declare foo as const volatile pointer to pointer to const volatile pointer to const volatile int
  ;; const volatile int (* const volatile (** const volatile foo))
  (is (equal "const volatile int *const volatile (*(*const volatile (~a)))"
             (paren::fmt-string<-type '(:cv-pointer (:pointer (:cv-pointer :cv-int))))))

  ;; declare foo as const volatile pointer to const int
  (is (equal "const int *const volatile (~a)"
             (paren::fmt-string<-type '(:cv-pointer :c-int))))

  ;; declare foo as const pointer to pointer to pointer to const int
  ;; const int (** (* const foo))
  (is (equal "const int **(*const (~a))"
             (paren::fmt-string<-type '(:c-pointer (:pointer :c-int 2)))))

  ;; declare foo as const pointer to function returning int
  ;; int ((* const foo)())
  (is (equal "int (*const (~a)) ()"
             (paren::fmt-string<-type '(:c-pointer (:function :int ()))))))

;; (test empty
;;   (c '())                               ;FIXME this has to emit an error ; TODO
;;   )

(test enum
  (is
   (string=
    "enum { sunday, monday, tuesday, wednesday, Thursday, FRiDAY, SATURDAY }"
    (c '(enum |Day| (sunday monday tuesday wednesday |Thursday| "FRiDAY" |saturday|))))))

(test union
  (is (string= (c '(union |Data| (i :int) (f :float) (str (:array 20 :char))))
               (format nil "~:
union Data {
  int i;
  float f;
  char (str)[20];
};"))))

(test indent
  (is
   (string=
    (format nil "~:
  1
  2

  3
  4

")
    (paren::indent (format nil "~:
1
2

3
4

"))))

  (is
   (string=
    (format nil "~:
    1
    2

    3
    4

")
    (paren::indent (format nil "~:
1
2

3
4

") :space-count 4))))

(test prefix-newline-with-backslash     ; util
  (is
   (string=
    (paren::prefix-newline-with-backslash "a

b")
    "a \\
 \\
b"))
  (is
   (string=
    (paren::prefix-newline-with-backslash "a

b
")
    "a \\
 \\
b \\
")))

(test invert-case                       ; util
  (is (string= "" (paren::invert-case "")))
  (is (string= "hello" (paren::invert-case "HELLO")))
  (is (string= "我hello" (paren::invert-case "我HELLO")))
  (is (string= "HELLO" (paren::invert-case "hello")))
  (is (string= "我HELLO" (paren::invert-case "我hello")))
  (is (string= "HelloWorld" (paren::invert-case "HelloWorld")))
  (is (string= "我HelloWorld" (paren::invert-case "我HelloWorld"))))

(test resolve-declaration               ; util
  (is (equal "int x"
             (paren::resolve-declaration '(x :int))))
  (is (equal "int x_y"
             (paren::resolve-declaration '(x-y :int))))
  (is (equal "int *(x)"
             (paren::resolve-declaration '(x (:pointer :int 1)))))
  (is (equal "char **(argv)"
             (paren::resolve-declaration '(argv (:pointer :char 2)))))
  (is (equal "x *(next)"
             (paren::resolve-declaration '(next (:pointer :x)))))
  (is (equal "struct x window"
             (paren::resolve-declaration '(window (:struct :x)))))
  (is (equal "struct X window"
             (paren::resolve-declaration '(window (:struct :|x|)))))
  (is (equal "struct xX window"
             (paren::resolve-declaration '(window (:struct :|xX|))))))

;; A util macro for testing.
(defmacro with-expected-warning (&body body)
  "Evaluate BODY, expecting a WARNING to be signaled during execution.
If no WARNING is signaled, an error is raised. The function
execution continues as normal, and the result of BODY is returned.

This macro ensures that:
1. At least one WARNING is signaled during BODY's execution.
2. Warnings are muffled, so they don't propagate beyond the macro.

Example usage:
  (with-expected-warning
    (warn \"This is a test warning.\")
    42) ; => 42

Signals an error if no warning occurs."
  `(let ((warning-signaled-p nil))
     (flet ((handler (c)
              (setf warning-signaled-p t)
              (muffle-warning c)))
       (handler-bind ((warning #'handler))
         (let ((result (progn ,@body)))
           (unless warning-signaled-p
             (error "A warning was expected, but none was signaled."))
           result)))))

(test symbols
  (is (equal ""
             (with-expected-warning
               (c 'a b))))
  (is (equal "abcde" (c 'abcde)))
  (is (equal "abCde" (c '|abCde|)))
  (is (equal "ABCDE" (c '|abcde|))))

(test compile-each
  (is
   (string=
    (format nil "~:
func1(1);
func2(2);
func3(3);")
    (c '(compile-each "" (@func1 1) (@func2 2) (@func3 3)))))

  ;; compile-each should add trailing semicolon when there's an expression.
  (is
   (string=
    "0;
1;
2;
3;
((4) + (5));"
    (c '(compile-each "" 0 1 2 3 (+ 4 5))))))

(test lisp                              ; interop
  (is
   (string=
    (format nil
            "~:
int foo_INT (int x, int y) {
  return (((2) * (((x) + (y)))));
}
float foo_FLOAT (float x, float y) {
  return (((2) * (((x) + (y)))));
}
double foo_DOUBLE (double x, double y) {
  return (((2) * (((x) + (y)))));
}")
    (c
     '(lisp
       (defun gen-foo (type)
         ;; paren code:
         `(defun (,(intern (format nil "foo-~a" type)) ,type) ((x ,type) (y ,type))
            (return (* 2 (+ x y)))))
       `(compile-each "" ,@(loop :for type :in '(:int :float :double)
                                 :collect (gen-foo type)))))
    )))

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
  int value;
  x *(next);
};")
       (c `(defstruct x
             ((value :int)
              (next (:pointer :x)))))
       )))

(test control-flow
  (is
   (string=
    "{
  if (((x) < (0))) {
    goto negative;
  }
  if (((y) < (0))) {
    {
      negative:
      printf(\"Negative\\n\");
      return;
    }
  }
}"
    (c
     '(block
       (cond ((< x 0)
              (goto negative)))
       (cond ((< y 0)
              (block
                  (label negative)
                (@printf (str "Negative\\n"))
                (return))))))
    )))

(test define
  (is
   (string=
    "#define kT 4
#define kQuote 6
"
    (c '(define |kT| 4 |kQuote| 6)))))

(test funcall
  (is (equal
       "printf(\"x = %d, y = %d \\n\", x, y)"
       (c `(@printf (str "x = %d, y = %d \\n") x y))))
  (is (equal
       "malloc(sizeof(x))"
       (c `(@malloc (@sizeof x))))))

(test cast
  (is
   (string=
    (c '(cast (:pointer :int) (@malloc (* size (@sizeof :int)))))
    "(int *)(malloc(((size) * (sizeof(int)))))")))

(test array
  (is (equal "{1, 2, 3}"
             (c `(vec 1 2 3))))
  (is (equal
       "int (x)[3] = {1, 2, 3};"
       ;; TODO Maybe vec should just be an alias to array.
       (c '(declare () (x (:array 3 :int)) (vec 1 2 3))))))

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
       "((i) == (0))"
       (c '(== i 0)))))

(test +-*/
  (is
   (string=
    (c '(+ i j 2 k 1))
    "((i) + (j) + (2) + (k) + (1))"))
  (is
   (string=
    (c '(- i j 2 k 1))
    "((i) - (j) - (2) - (k) - (1))"))
  (is
   (string=
    (c '(* i j 2 k 1))
    "((i) * (j) * (2) * (k) * (1))"))
  (is
   (string=
    (c '(/ i j 2 k 1))
    "((i) / (j) / (2) / (k) / (1))"))
  (is
   (string=
    (c '(++ i))
    "((i)++)"))
  (is
   (string=
    (c '(++ i :pre))
    "(++(i))"))
  (is
   (string=
    (c '(-- i :pre))
    "(--(i))")))

(test or
  (is (equal
       "((((i) == (15))) || (((i) == (20))))"
       (c `(or (== i 15)
               (== i 20))))))

(test and
  (is (equal
       "((((i) > (0))) && (((i) < (30))))"
       (c `(and (> i 0)
                (< i 30))))))

(test include
  (is (equal
       (format nil "~:
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include \"emacs.h\"")
       (c `(include :system ("stdio.h" "stdlib.h" "math.h")
                    :local ("emacs.h")))
       )))

(test declare
  (is (equal
       "int x;"
       (c '(declare () (x :int)))))
  (is (equal
       "int x = {1, 2, 3};"
       (c '(declare () (x :int) (vec 1 2 3)))))
  (is (equal
       "int *(x);"
       (c '(declare () (x (:pointer :int 1))))))
  (is (equal
       "int *(x) = 42;"
       (c '(declare () (x (:pointer :int 1)) 42)))))

(test declare/multiple
  (is
   (string=
    "int x = 1;
int y = 2;
int z = 3;"
    (c '(declare () (x :int) 1 (y :int) 2 (z :int) 3))))
  (is
   (string=
    "register int x = 1;
register int y = 2;
register int z = 3;"
    (c '(declare :register (x :int) 1 (y :int) 2 (z :int) 3))))
  (is
   (equal
    (paren::c-expand-1 '(declare :register (x :int) 1 (y :int) 2 (z :int) 3))
    '(paren::compile-each ""
      (declare :register (x :int) 1)
      (declare :register (y :int) 2)
      (declare :register (z :int) 3))))
  (is
   (equal
    (paren::c-expand-1 '(declare () (x :int) 1 (y :int) 2 (z :int) 3))
    '(paren::compile-each ""
      (declare :auto (x :int) 1)
      (declare :auto (y :int) 2)
      (declare :auto (z :int) 3)))))

(test declare/storage-class
  (is (equal
       "int x;"
       (c '(declare () (x :int)))))
  (is (equal
       "int x;"
       (c '(declare :auto (x :int)))))
  (is (equal
       "register int x;"
       (c '(declare :register (x :int)))))
  (is (equal
       "static int x;"
       (c '(declare :static (x :int)))))
  (is (equal
       "extern int x;"
       (c '(declare :extern (x :int)))))
  (is (equal
       "static int *((x)[9]) = 42;"
       (c '(declare :static (x (:array 9 (:pointer :int 1))) 42)))))

(test while
  (is
   (string=
    (c '(while x (set x (@ |m| (++ i)))))
    "while (x) {
  x = M[((i)++)];
}")))

(test do-while
  (is
   (string=
    "do {
  printf(\"Hello\");
  printf(\"World\");
} while (((x) > (y)));"
    (c '(do-while (> x y)
         (@printf (str "Hello"))
         (@printf (str "World")))))))

(test ref/deref
  (is
   (string=
    "&a"
    (c '(& a))))
  (is
   (string=
    "*ptr"
    (c '(deref ptr))))
  (is
   (string=
    "*((ptr)++)"
    (c '(deref (++ ptr)))))
  )

(test exit
  (is
   (string=
    "exit(0);"
    (c '(exit 0))))
  (is
   (string=
    "exit(((1) + (1)));"
    (c '(exit (+ 1 1))))))

(test break
  (is
   (string=
    "if (((1) == (1))) {
  break;
}"
    (c '(cond ((== 1 1) (break)))))))

(test set
  (is (equal
       (c `(set x 10))
       "x = 10"))
  (is (equal
       (c `(set (-> x1 value) 10))
       "x1->value = 10")))

(test for
  (is
   (string=
    (format nil
            "~:
for (size_t i = 0; ((i) < (size)); ((i)++)) {
  printf(\"%d\", result[((i) + (1))]);
  printf(\"%d\", result[i]);
}")
    (c '(for ((declare () (i :size-t) 0)
              (< i size)
              (++ i))
         (@printf (str "%d") (@ result (+ i 1)))
         (@printf (str "%d") (@ result i))))))

  (is
   (string=
    (format nil "~:
for (int i = 0; ((i) < (3)); ((i)++)) {
  for (int j = 0; ((j) < (2)); ((j)++)) {
    for (int k = 0; ((k) < (2)); ((k)++)) {
      printf(\"Hello, \");
      printf(\"world!\");
      printf(\"%d\", k);
    }
    printf(\"%d\", j);
  }
  printf(\"%d\", i);
}")
    (c '(lisp
         (defun multi-for (bindings body)
           (loop :for binding :in (reverse bindings)
                 :do (setf body `((for ((declare () (,(nth 0 binding) :int) 0)
                                        (< ,(nth 0 binding) ,(nth 1 binding))
                                        (++ ,(nth 0 binding)))
                                   ,@body
                                   (@printf (str "%d") ,(nth 0 binding))))))
           (car body))
         (multi-for '((i 3) (j 2) (k 2))
          '((@printf (str "Hello, "))
            (@printf (str "world!"))))))))
  (is
   (string=
    "for (i = 0; x = M[((i)++)]; ) {

}"
    (c '(for ((set i 0) (set x (@ |m| (++ i))) ())))))

  (is
   (string=
    "for (size_t i = 0; ((i) < (size)); ((i)++)) {
  result[i] = f(arr[i]);
}"
    (c '(for ((declare () (i :size-t) 0)
              (< i size)
              (++ i))
         (set (@ result i) (@f (@ arr i))))))))

(test @
    (is (string=
         (c '(@ result i))
         "result[i]"))
  (is (string=
       (c '(@ result (+ i j)))
       "result[((i) + (j))]")))

(test if
  (is
   (eq :error
       (handler-case
           ;; The form is expected to throw an error because the length of form should be either 2 or 3.
           (c '(if (< x 0)))
         (error (e) (declare (ignore e)) :error))))

  (is
   (eq :error
       (handler-case
           ;; The form is expected to throw an error because the length of form should be either 2 or 3.
           (c '(if (< x 0) 1 2 3))
         (error (e) (declare (ignore e)) :error))))

  (is
   (string=
    "if (((x) < (0))) {
  goto negative;
}"

    (c '(if (< x 0) (goto negative)))))

  (is
   (string=
    "if (((x) < (0))) {
  goto negative;
} else {
  goto positive;
}"
    (c '(if (< x 0) (goto negative) (goto positive))))))

(test unless
  (is
   (string=
    "if ((!(1))) {
  2;
  3;
  4;
  5;
}"
    (c '(unless 1 2 3 4 5)))))

(test when
  (is
   (string=
    "if (1) {
  2;
  3;
  4;
  5;
}"
    (c '(when 1 2 3 4 5)))))

(test ?
  (is
   (string=
    "((0) ? (1) : (2))"
    (c '(? 0 1 2))))
  (is
   (string=
    "return (((((car(x)) < (0))) ? (0) : (kT)));"
    (c '(return (? (< (@car x) 0)
                 0
                 |kT|))))))

(test cond
  (is
   (string=
    (format nil
            "~:
if (((i) == (10))) {
  printf(\"Hello!\\n\");
  printf(\"i is 10\\n\");
} else if ((((i) == (15))) || (((i) == (20)))) {
  printf(\"i is 15 or 20\\n\");
} else if ((((i) > (0))) && (((i) < (30)))) {
  printf(\"i is between 0 and 30\\n\");
} else {
  printf(\"i is not present\\n\");
}")
    (c '(cond
         ((== i 10)
          (@printf (str "Hello!\\n"))
          (@printf (str "i is 10\\n")))
         ((or (== i 15)
           (== i 20))
          (@printf (str "i is 15 or 20\\n")))
         ((and (> i 0)
           (< i 30))
          (@printf (str "i is between 0 and 30\\n")))
         (t
          (@printf (str "i is not present\\n"))))))
   ))

;; TODO Need defun-with-specified-storage. I don't want to clutter the interface of defun anymore.
(test defun
  (is (equal
       (format nil "~:
int main () {
  int x = 5;
  int y = 10;
  return (0);
}")
       (c `(defun (main :int) ()
             (declare () (x :int) 5)
             (declare () (y :int) 10)
             (return 0)))))

  (is (equal
       (format nil "~:
int main (int argc, char **(argv)) {
  return (0);
}")
       (c `(defun (main :int) ((argc :int) (argv (:pointer :char 2)))
             (return 0))))))

(test assignment
  (is (equal
       "int x = 5;"
       (c `(declare () (x :int) 5))))
  (is (equal
       "int y = 10;"
       (c `(declare () (y :int) 10)))))

(test case
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
    return (1);
    break;
  case 2:
    return (2);
    break;
  default:
    return (0);
    break;
}"))))

(test return
  (is (equal
       "return (0);"
       (c `(return 0))))
  (is (equal
       "return (((1) + (1)));"
       (c `(return (+ 1 1))))))

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
  ;; NOTE Handy for users who just want to insert some code without learning paren the language.
  (is (equal
       (format nil "~:
int main () {
  int (*(*foo)(void ))[3];
  const int (* volatile bar)[64];
  (double (^)(int , long long ))baz;
  return (0);
}")
       (c `(defun (main :int) ()
             "int (*(*foo)(void ))[3];"
             "const int (* volatile bar)[64];"
             "(double (^)(int , long long ))baz;"
             (return 0)))
       )))

(test integration-test?
  (is
   (string=
    (c `(@scanf (str "%d") (& day)))
    "scanf(\"%d\", &day)"))
  (is
   (string=
    (c `(@printf (str "x = %d, y = %d \\n") x y))
    "printf(\"x = %d, y = %d \\n\", x, y)")))



;;;

;; util for the following tests
(defun string-diff (str1 str2)
  "Compares two strings character by character, tracking line and column.
   Returns (:line LINE :char CHAR :char1 CHAR1 :char2 CHAR2) for the first
   differing character, or NIL if they are identical."
  (let ((line 1)
        (char 1))
    (loop :for c1 :across str1
          :for c2 :across str2
          :do (cond
                ;; If characters differ, return the report
                ((not (char= c1 c2))
                 (return `(:line ,line :char ,char :char-1 ,c1 :char-2 ,c2)))
                ;; Update line and char counters
                ((char= c1 #\Newline)
                 (incf line)
                 (setf char 0)))
          :do (incf char))))

;; TODO Make these tests.
(string-diff "a" "a")
(string-diff "ab" "ac")
(string-diff (format nil "a~%~%b")
             (format nil "a~%b"))
(string-diff (format nil "a~%b ~%c")
             (format nil "a~%b~%c"))

(defun compilation-diff? (lsp-file-path)
  (let ((diff (string-diff
               (with-output-to-string (s)
                 (paren::compile-lsp-forms
                  (paren::read-file-into-list
                   lsp-file-path)
                  :stream s))
               (paren::read-file-to-string
                (paren::c-path lsp-file-path)))))
    (when diff (log:info diff
                         lsp-file-path
                         (paren::c-path lsp-file-path)))
    diff))

(test compilation-difference?
  (is (not (compilation-diff? "../examples/hello-world.lsp")))
  (is (not (compilation-diff? "../examples/switch.lsp")))
  (is (not (compilation-diff? "../examples/cond.lsp")))
  (is (not (compilation-diff? "../examples/control-flow.lsp")))
  (is (not (compilation-diff? "../examples/macro-example.lsp")))
  (is (not (compilation-diff? "../examples/type-struct-example.lsp")))
  (is (not (compilation-diff? "../examples/higher-order-function.lsp")))
  (is (not (compilation-diff? "../examples/nested-loops.lsp")))
  (is (not (compilation-diff? "../examples/c-macro.lsp"))))

;;;

(defparameter *root-dir* "/tmp/paren/")

(defun compile-c-file (c-file)
  (ensure-directories-exist *root-dir*)
  (let ((c-file-absolute (truename c-file))
        (out-file
          (format nil "~a"
                  (merge-pathnames (format nil "~a.~a.o"
                                           (pathname-name c-file)
                                           (get-universal-time))
                                   *root-dir*))))
    (log:debug "Compiling ~a to ~a ..~%  c-file-absolute: ~a" c-file out-file c-file-absolute)
    ;; NOTE This depends on sbcl.
    (let* ((exit-code)
           (stdout/err
             (with-output-to-string (s)
               (setf exit-code
                     (sb-ext:process-exit-code
                      (sb-ext:run-program "/usr/bin/gcc" `(,c-file "-o" ,out-file)
                                          :output s :error s))))))
      (unless (= 0 exit-code)
        (error "~%GCC compilation process failed. STDOUT/ERR:~%~a" stdout/err)))
    out-file))

(sb-ext:process-exit-code
 (sb-ext:run-program "/bin/echo" `()))

(defun run-program (command &key (input ""))
  (let ((stdin (make-string-input-stream input))
        (stdout (make-string-output-stream))
        (stderr (make-string-output-stream)))
    (log:debug "Running command ~s with input ~s." command input)
    ;; NOTE This depends on sbcl.
    (sb-ext:run-program command nil
                        :input stdin
                        :output stdout
                        :error stderr)
    (cons (get-output-stream-string stdout)
          (get-output-stream-string stderr))))

(defun test-c-program (c-file &key (stdin "") (expected-stdout "") (expected-stderr ""))
  (let* ((result (run-program (compile-c-file c-file) :input stdin))
         (stdout (car result))
         (stderr (cdr result))
         (expected-stdout (format nil expected-stdout))
         (expected-stderr (format nil expected-stderr)))
    (unless (string= expected-stdout stdout)
      (error "~:
Error: Unmatched STDOUT.
       c-file: ~a
       stdin: ~a~%

-----Expected-----
~a
------We Got------
~a
------------------ "
             c-file stdin
             expected-stdout stdout))
    (unless (string= expected-stderr stderr)
      (error "~:
Error: Unmatched STDERR.
       c-file: ~a
       stdin: ~a~%

-----Expected-----
~a
------We Got------
~a
------------------"
             c-file stdin
             expected-stderr stderr))
    t))


(test execution-test

  (is
   (test-c-program "../examples/macro-example.c"
                   :stdin ""
                   :expected-stdout "~:
foo_INT(5, 10) = 30
foo_FLOAT(2.50, 3.50) = 12.00
foo_DOUBLE(1.234, 4.567) = 11.602
"
                   :expected-stderr ""))

  (is
   (test-c-program "../examples/higher-order-function.c"
                   :stdin ""
                   :expected-stdout "1 4 9 16 25 36 49 64 81 100 "
                   :expected-stderr ""))

  (is
   (test-c-program "../examples/nested-loops.c"
                   :stdin ""
                   :expected-stdout "~:
0 0
1 0
1 0
2 0
1 1
2 1
2 1
3 1
2 2
3 2
3 2
4 2
"
                   :expected-stderr ""))

  (is
   (test-c-program "../examples/control-flow.c"
                   :stdin "1 1"
                   :expected-stdout
                   "~:
Enter an integer for x:
Enter an integer for y:
You entered x = 1 and y = 1
x is positive.
"
                   :expected-stderr ""))

  (is
   (test-c-program "../examples/control-flow.c"
                   :stdin "1 -1"
                   :expected-stdout
                   "~:
Enter an integer for x:
Enter an integer for y:
You entered x = 1 and y = -1
x is positive.
y is negative.
"
                   :expected-stderr ""))

  (is
   (test-c-program "../examples/control-flow.c"
                   :stdin "-1 1"
                   :expected-stdout
                   "~:
Enter an integer for x:
Enter an integer for y:
You entered x = -1 and y = 1
x is negative.
"
                   :expected-stderr ""))

  (is
   (test-c-program "../examples/control-flow.c"
                   :stdin "-1 -1"
                   :expected-stdout
                   "~:
Enter an integer for x:
Enter an integer for y:
You entered x = -1 and y = -1
x is negative.
"
                   :expected-stderr ""))
  ;;

  (is
   (test-c-program "../examples/cond.c"
                   :stdin ""
                   :expected-stdout
                   "Hello!~%i is 15 or 20~%"
                   :expected-stderr ""))
  ;;

  (is
   (test-c-program "../examples/hello-world.c"
                   :stdin ""
                   :expected-stdout "Hello, world!"
                   :expected-stderr ""))
  ;;

  (is
   (test-c-program "../examples/type-struct-example.c"
                   :stdin ""
                   :expected-stdout "~:
x1->value             = 10
x1->next->value       = 20
x1->next->next->value = 30
"
                   :expected-stderr ""))

  ;;

  (is
   (test-c-program "../examples/c-macro.c"
                   :stdin ""
                   :expected-stdout
                   "x = 10, y = 5~%"
                   :expected-stderr ""))
  ;;

  (is
   (test-c-program "../examples/switch.c"
                   :stdin "1"
                   :expected-stdout
                   "Enter an integer for i: i = 1~%"
                   :expected-stderr ""))

  (is
   (eq :error
       (handler-case
           (test-c-program "../examples/switch.c"
                           :stdin "1"
                           :expected-stdout
                           "Some randomly written stdout."
                           :expected-stderr "")
         (error (e) (declare (ignore e)) :error))))

  (is
   (test-c-program "../examples/switch.c"
                   :stdin "3 1"
                   :expected-stdout
                   "Enter an integer for i: i = 3~%Enter an integer for j: j = 1~%"
                   :expected-stderr ""))

  (is
   (test-c-program "../examples/switch.c"
                   :stdin "4"
                   :expected-stdout
                   "Enter an integer for i: Wrong guess. Aborting..~%"
                   :expected-stderr ""))

  (is
   (test-c-program "../examples/sectorlisp/lisp.c"
                   :stdin "(EQ (QUOTE T) (QUOTE F)) "
                   :expected-stdout "NIL~%"))
  (is
   (test-c-program "../examples/sectorlisp/lisp.c"
                   :stdin "
  (EQ (QUOTE T) (QUOTE F))
  (EQ (QUOTE A) (QUOTE A)) "
                   :expected-stdout "NIL~%T~%"))

  (is
   (test-c-program "../examples/sectorlisp/lisp.c"
                   :stdin "
  ((LAMBDA (FF X) (FF X))
   (QUOTE (LAMBDA (X)
            (COND ((ATOM X) X)
                  ((QUOTE T) (FF (CAR X))))))
   (QUOTE ((A) B C))) "
                   :expected-stdout "A~%"))

  (is
   (test-c-program "../examples/sectorlisp/lisp.c"
                   :stdin "
  ((LAMBDA (ASSOC EVCON PAIRLIS EVLIS APPLY EVAL)
     (EVAL (QUOTE ((LAMBDA (FF X) (FF X))
                   (QUOTE (LAMBDA (X)
                            (COND ((ATOM X) X)
                                  ((QUOTE T) (FF (CAR X))))))
                   (QUOTE ((A) B C))))
           ()))
   (QUOTE (LAMBDA (X Y)
            (COND ((EQ Y ()) ())
                  ((EQ X (CAR (CAR Y)))
                         (CDR (CAR Y)))
                  ((QUOTE T)
                   (ASSOC X (CDR Y))))))
   (QUOTE (LAMBDA (C A)
            (COND ((EVAL (CAR (CAR C)) A)
                   (EVAL (CAR (CDR (CAR C))) A))
                  ((QUOTE T) (EVCON (CDR C) A)))))
   (QUOTE (LAMBDA (X Y A)
            (COND ((EQ X ()) A)
                  ((QUOTE T) (CONS (CONS (CAR X) (CAR Y))
                                   (PAIRLIS (CDR X) (CDR Y) A))))))
   (QUOTE (LAMBDA (M A)
            (COND ((EQ M ()) ())
                  ((QUOTE T) (CONS (EVAL (CAR M) A)
                                   (EVLIS (CDR M) A))))))
   (QUOTE (LAMBDA (FN X A)
            (COND
              ((ATOM FN)
               (COND ((EQ FN (QUOTE CAR))  (CAR  (CAR X)))
                     ((EQ FN (QUOTE CDR))  (CDR  (CAR X)))
                     ((EQ FN (QUOTE ATOM)) (ATOM (CAR X)))
                     ((EQ FN (QUOTE CONS)) (CONS (CAR X) (CAR (CDR X))))
                     ((EQ FN (QUOTE EQ))   (EQ   (CAR X) (CAR (CDR X))))
                     ((QUOTE T)            (APPLY (EVAL FN A) X A))))
              ((EQ (CAR FN) (QUOTE LAMBDA))
               (EVAL (CAR (CDR (CDR FN)))
                     (PAIRLIS (CAR (CDR FN)) X A))))))
   (QUOTE (LAMBDA (E A)
            (COND
              ((ATOM E) (ASSOC E A))
              ((ATOM (CAR E))
               (COND ((EQ (CAR E) (QUOTE QUOTE)) (CAR (CDR E)))
                     ((EQ (CAR E) (QUOTE COND)) (EVCON (CDR E) A))
                     ((QUOTE T) (APPLY (CAR E) (EVLIS (CDR E) A) A))))
              ((QUOTE T) (APPLY (CAR E) (EVLIS (CDR E) A) A)))))) "
                   :expected-stdout "A~%"))

  ;; FIXME We are working on sectorlisp.c.
  ;;
  (is
   (test-c-program "../examples/sectorlisp.c"
                   :stdin "(A A)"
                   :expected-stdout "A~%"))
  )
