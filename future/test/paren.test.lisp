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
  (is (paren::type? '(:function :int (:int (:pointer (:array () :int) 1)))))
  (is (not (paren::type? '(:pointer :int 0))))
  (is (not (paren::type? '(:array 0 :int))))
  (is (not (paren::type? '(:array 1 (:array 0 :int))))))

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



  ;; FIXME TODO Do I really want to use :c-int? Cuz then I still need to
  ;; implement c-char, c-double.. etc. It's not composable.

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

(test symbols
  (is (equal "abcde" (c 'abcde)))
  (is (equal "abCde" (c '|abCde|)))
  (is (equal "ABCDE" (c '|abcde|))))

(test prog-badname
  (is
   (string=
    (c '(progn-badname (@func1 1) (@func2 2) (@func3 3)))
    (format nil "~:
func1(1);
func2(2);
func3(3);"))))

(test lisp                              ; interop
  (is
   (string=
    (c
     '(lisp
       (defun gen-foo (type)
         ;; paren code:
         `(defun (,(intern (format nil "foo-~a" type)) ,type) ((x ,type) (y ,type))
            (return (* 2 (+ x y)))))
       `(progn-badname ,@(loop :for type :in '(:int :float :double)
                               :collect (gen-foo type)))))

    (format nil
            "~:
int foo_INT (int x, int y) {
  return (((2) * (((x) + (y)))));
};
float foo_FLOAT (float x, float y) {
  return (((2) * (((x) + (y)))));
};
double foo_DOUBLE (double x, double y) {
  return (((2) * (((x) + (y)))));
};")                                    ; FIXME Fix semicolon problem.. or is there not problem? idk..
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
    (c
     '(block
       (cond ((< x 0)
              (goto negative)))
       (cond ((< y 0)
              (block
                (label negative)
                (@printf (str "Negative\\n"))
                (return))))))
    "{
  if ((x) < (0)) {
    goto negative;
  };
  if ((y) < (0)) {
    {
      negative:
      printf(\"Negative\\n\");
      return;
    };
  };
}"
    )))

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
    "(int *(~a))(malloc(((size) * (sizeof(int)))))")))

(test array
  (is (equal "{1, 2, 3}"
             (c `(vec 1 2 3))))
  (is (equal
       "int (x)[3] = {1, 2, 3}"
       ;; TODO Maybe vec should just be an alias to array.
       (c '(declare (x (:array 3 :int)) (vec 1 2 3))))))

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
    (c '(++ i))
    "((i)++)"))
  (is
   (string=
    (c '(-- i))
    "((i)--)")))

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
       "int x"
       (c '(declare (x :int)))))
  (is (equal
       "int x = {1, 2, 3}"
       (c '(declare (x :int) (vec 1 2 3)))))
  (is (equal
       "int *(x)"
       (c '(declare (x (:pointer :int 1))))))
  (is (equal
       "int *(x) = 42"
       (c '(declare (x (:pointer :int 1)) 42)))))

(test set
  (is (equal
       (c `(set x 10))
       "x = 10"))
  (is (equal
       (c `(set (-> x1 value) 10))
       "x1->value = 10")))

(test for
  (string=
   (c '(for ((declare (i :size-t) 0)
             (< i size)
             (++ i))
        (@printf (str "%d") (@ result (+ i 1)))
        (@printf (str "%d") (@ result i))))
   (format nil
           "~:
for (size-t (i) = 0; (i < size); (i++)) {
  printf(\"%d\", (result[((i)+(1))]))
  printf(\"%d\", (result[i]))
}")))

(test @
    (is (string=
         (c '(@ result i))
         "result[i]"))
    (is (string=
         (c '(@ result (+ i j)))
         "result[((i) + (j))]")))

(test cond
  (is
   (string=
    (format nil
            "~:
if ((i) == (10)) {
  printf(\"i is 10\\n\");
} else if ((((i) == (15))) || (((i) == (20)))) {
  printf(\"i is 15 or 20\\n\");
} else if ((((i) > (0))) && (((i) < (30)))) {
  printf(\"i is between 0 and 30\\n\");
} else {
  printf(\"i is not present\\n\");
}")

    (c '(cond ((== i 10)
               (@printf (str "i is 10\\n")))
         ((or (== i 15)
           (== i 20))
          (@printf (str "i is 15 or 20\\n")))
         ((and (> i 0)
           (< i 30))
          (@printf (str "i is between 0 and 30\\n")))
         (t
          (@printf (str "i is not present\\n"))))))))

(test defun
  (is (equal
       (format nil "~:
int main () {
  int x = 5;
  int y = 10;
  return (0);
}")
       (c `(defun (main :int) ()
             (declare (x :int) 5)
             (declare (y :int) 10)
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
       "int x = 5"
       (c `(declare (x :int) 5))))
  (is (equal
       "int y = 10"
       (c `(declare (y :int) 10)))))

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
       "return (0)"
       (c `(return 0))))
  (is (equal
       "return (((1) + (1)))"
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
    (c `(@printf (str "x = %d, y = %d \\n") x y))
    "printf(\"x = %d, y = %d \\n\", x, y)")))

;;;

(defun compilation-diff? (lsp-file-path)
  ;; TODO Report where the first difference is (line number, which character).
  (not
   (string=
    (with-output-to-string (s)
      (paren::compile-lsp-forms (paren::read-file-into-list lsp-file-path)
                                :stream s))
    (paren::read-file-to-string (paren::c-path lsp-file-path)))))

(test compilation-difference?
  (is (not (compilation-diff? "./examples/hello-world.lsp")))
  (is (not (compilation-diff? "./examples/switch.lsp")))
  (is (not (compilation-diff? "./examples/cond.lsp")))
  (is (not (compilation-diff? "./examples/control-flow.lsp")))
  (is (not (compilation-diff? "./examples/macro-example.lsp")))
  (is (not (compilation-diff? "./examples/type-struct-example.lsp")))
  (is (not (compilation-diff? "./examples/higher-order-function.lsp")))
  (is (not (compilation-diff? "./examples/c-macro.lsp"))))

;;;

(defparameter *root-dir* "/tmp/paren/")

(defun compile-c-file (c-file)
  (ensure-directories-exist *root-dir*)
  (let ((out-file
          (format nil "~a"
                  (merge-pathnames (format nil "~a.~a.o"
                                           (pathname-name c-file)
                                           (get-universal-time))
                                   *root-dir*))))
    ;; TODO Resolve c-file to absolute path so it's easier to report when there's error.
    ;; NOTE This depends on sbcl.
    (log:info "Compiling ~a to ~a .." c-file out-file)
    (sb-ext:run-program "/usr/bin/gcc" `(,c-file "-o" ,out-file))
    out-file))

(defun run-program (command &key (input ""))
  (let ((stdin (make-string-input-stream input))
        (stdout (make-string-output-stream))
        (stderr (make-string-output-stream)))
    (log:info "Running command ~s with input ~s." command input)
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

------Expect------
~a
------We Get------
~a
------------------ "
             c-file stdin
             expected-stdout stdout))
    (unless (string= expected-stderr stderr)
      (error "~:
Error: Unmatched STDERR.
       c-file: ~a
       stdin: ~a~%

------Expect------
~a
------We Get------
~a
------------------"
             c-file stdin
             expected-stderr stderr))
    t))


(test execution-test
  (is
   (test-c-program "./examples/switch.c"
                   :stdin "1"
                   :expected-stdout
                   "Enter an integer for i: i = 1~%"
                   :expected-stderr ""))

  (is
   (test-c-program "./examples/switch.c"
                   :stdin "3\\n1"
                   :expected-stdout
                   "Enter an integer for i: i = 3~%Enter an integer for j: j = 1~%"
                   :expected-stderr ""))

  (is
   (test-c-program "./examples/switch.c"
                   :stdin "4"
                   :expected-stdout
                   "Enter an integer for i: Wrong guess. Aborting..~%"
                   :expected-stderr ""))
  ;; TODO The following test is destined to fail. Add it later.
  ;; (is
  ;;  (test-c-program "./examples/switch.c"
  ;;                  :stdin "1"
  ;;                  :expected-stdout
  ;;                  "Enter an integer for i: Wrong guess. Aborting..~%"
  ;;                  :expected-stderr ""))
  )
