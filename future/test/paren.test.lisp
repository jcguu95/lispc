(in-package :paren.test)
(setf (readtable-case *readtable*) :invert)
(def-suite paren.test)
(in-suite paren.test)

;;;

;; TODO Other keywords, C preprocessor macros, `int x[8]`, `int x[]`.

(test invert-case
  (is (string= (paren::invert-case "AbC") "aBc"))
  (is (string= (paren::invert-case "abc") "ABC"))
  (is (string= (paren::invert-case "ABC") "abc")) ; FIXME
  (is (string= (paren::invert-case "Ab1") "aB1")))

(test typedef
  (is (equal (c `(deftype (struct :x) x))
             "typedef struct X X;"))
  (is (equal (c `(deftype (struct :Y) Y))
             "typedef struct y y;")))

(test defstruct
  (is (equal
       (c `(defstruct X
             ((value :int)
              (next (* :X)))))
       "struct X {
  int value;
  X *next;
};")))

(test funcall
  (is (equal
       (c `(@printf (str "x = %d, y = %d \\n") X Y))
       "printf(\"x = %d, y = %d \\n\", x, y)"))
  (is (equal
       (c `(@malloc (@sizeof x)))
       "malloc(sizeof(X))")))

(test ->
  (is (equal
       (c `(-> x1 value))
       "x1->value"))
  (is (equal
       (c `(-> x1 next next value))
       "x1->next->next->value"))
  (is (equal
       (c `(@printf (str "Answer: %d.\\n") (-> x1 next next value)))
       "printf(\"Answer: %d.\\n\", x1->next->next->value)")))

(test string
  (is (equal
       (c `(str "ABCd"))
       "\"ABCd\""))
  ;; FIXME Why is this wrong?
  (is (equal
       (c `(str "ABC"))
       "\"ABC\"")))

(test ==
  (is (equal
       (c `(== I 0))
       "(i == 0)")))

(test or
  (is (equal
       (c `(or (== I 15)
               (== I 20)))
       "((i == 15) || (i == 20))")))

(test and
  (is (equal
       (c `(and (> I 0)
                (< I 30)))
       "((i > 0) && (i < 30))")))

(test include
  (is (equal
       (c `(include :system ("stdio.h" "stdlib.h" "math.h")
                    :local ("emacs.h")))
       "#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include \"emacs.h\"
")))

(test type
  (is (equal
       (c '(set (x :int)))
       "int x"))
  (is (equal
       (c '(set (x (* 1 :int))))
       "int *x"))
  (is (equal
       (c '(set (x (* 2 :int))))
       "int **x"))
  (is (equal
       (c `(defun (main :int) ((argc :int) (argv (* 2 :char)))
             (return 0)))
       "int main (int argc, char **argv) {
  return 0;
}")))


(test set
  (is (equal
       (c '(set (x :int)))
       "int x"))
  (is (equal
       (c '(set (x (* 1 :int))))
       "int *x"))
  (is (equal
       (c '(set (x (* 1 :int)) 42))
       "int *x = 42")))

(test defun
  (is (equal
       (c `(defun (main :int) ()
             (set (x :int) 5)
             (set (y :int) 10)
             (return 0)))
       "int main () {
  int x = 5;
  int y = 10;
  return 0;
}")))

(test assignment
  (is (equal
       (c `(set (x :int) 5))
       "int x = 5"))
  (is (equal
       (c `(set (y :int) 10))
       "int y = 10")))

(test return
  (is (equal
       (c `(return 0))
       "return 0")))
