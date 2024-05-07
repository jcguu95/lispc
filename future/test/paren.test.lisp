(in-package :paren.test)
;; (setf (readtable-case *readtable*) :invert)
(def-suite paren.test)
(in-suite paren.test)

;;;

;; TODO Other keywords, C preprocessor macros, `int x[8]`, `int x[]`.

(test invert-case                       ; util
  (is (string= "aBc" (paren::invert-case "AbC")))
  (is (string= "abc" (paren::invert-case "ABC")))
  (is (string= "aB1" (paren::invert-case "Ab1")))
  (is (string= "ABCD" (paren::invert-case "abcd")))
  ;; FIXME Why is this wrong? Very weird bug.. It is correct while manually
  ;; evaluated in the buffer. And it seems to fail only for the string "abc".
  ;;
  ;; (is (string= "ABC" (paren::invert-case "abc")))
  )

(test resolve-type ; util
  (is (equal "int"
             (paren::resolve-type :int)))
  (is (equal '(:pointer "int" 2)
             (paren::resolve-type '(* 2 :int))))
  (is (equal '(:pointer "int" 1)
             (paren::resolve-type '(* 1 :int))))
  (is (equal '(:pointer "int" 1)
             (paren::resolve-type '(*   :int))))
  (is (equal '(:struct "int")
             (paren::resolve-type '(:struct :int)))))

(test resolve-declaration               ; util
  (is (equal "int x"
             (paren::resolve-declaration '(x :int))))
  (is (equal "int x_y"
             (paren::resolve-declaration '(x-y :int))))
  (is (equal "int X"
             (paren::resolve-declaration '(X :int))))
  (is (equal "int *X"
             (paren::resolve-declaration '(X (* 1 :int)))))
  (is (equal "char **argv"
             (paren::resolve-declaration '(argv (* 2 :char)))))
  (is (equal "X *next"
             (paren::resolve-declaration '(next (* :X)))))
  (is (equal "struct X window"
             (paren::resolve-declaration '(window (:struct :X)))))
  (is (equal "struct X ***a_b_c"
             (paren::resolve-declaration '(a-b-c (* 3 (:struct :X))))))
  ;; NOTE By default, we set reader case to be :INVERT for the user to write
  ;; lisp code in lower case. In paren.lisp, we invert the case back. But
  ;; :INVERT only inverts the case only for symbols all of whose unescaped
  ;; characters are of the same case, so the behavior for symbols with mixed
  ;; cases are inverted.
  (is (equal "int A_b_C_d_E"
             (paren::resolve-declaration '(a-B-c-D-e :int))))
  (is (equal "int AbCdE1"
             (paren::resolve-declaration '(aBcDe1 :int))))
  )

(test typedef
  (is (equal "typedef struct x x;"
             (c `(deftype (:struct :x) x))))
  (is (equal "typedef struct Y Y;"
             (c `(deftype (:struct :Y) Y)))))

(test defstruct
  (is (equal "struct X {
  int value;
  X *next;
};"
             (c `(defstruct X
                   ((value :int)
                    (next (* :X)))))
             )))

(test funcall
  (is (equal
       "printf(\"x = %d, y = %d \\n\", x, y)"
       (c `(@printf (str "x = %d, y = %d \\n") x y))))
  (is (equal
       "malloc(sizeof(x))"
       (c `(@malloc (@sizeof x))))))

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
       "#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include \"emacs.h\"
"
       (c `(include :system ("stdio.h" "stdlib.h" "math.h")
                    :local ("emacs.h")))
       )))

(test type
  (is (equal
       "int x"
       (c '(set (x :int)))))
  (is (equal
       "int *x"
       (c '(set (x (* 1 :int))))))
  (is (equal
       "int **x"
       (c '(set (x (* 2 :int))))))
  (is (equal
       "int main (int argc, char **argv) {
  return 0;
}"
       (c `(defun (main :int) ((argc :int) (argv (* 2 :char)))
             (return 0))))))


(test set
  (is (equal
       "int x"
       (c '(set (x :int)))))
  (is (equal
       "int *x"
       (c '(set (x (* 1 :int))))))
  (is (equal
       "int *x = 42"
       (c '(set (x (* 1 :int)) 42)))))

(test defun
  (is (equal
       "int main () {
  int x = 5;
  int y = 10;
  return 0;
}"
       (c `(defun (main :int) ()
             (set (x :int) 5)
             (set (y :int) 10)
             (return 0))))))

(test assignment
  (is (equal
       "int x = 5"
       (c `(set (x :int) 5))))
  (is (equal
       "int y = 10"
       (c `(set (y :int) 10)))))

(test return
  (is (equal
       "return 0"
       (c `(return 0)))))
