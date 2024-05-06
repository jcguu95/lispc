(in-package :paren.test)
(setf (readtable-case *readtable*) :invert)
(def-suite paren.test)
(in-suite paren.test)

;;;

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

(test malloc-size-of
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
       (c `(str "ABC"))
       "\"ABC\"")))

(test ==
  (is (equal
       (c `(== i 0))
       "(i == 0)")))

(test or
  (is (equal
       (c `(or (== i 15)
               (== i 20)))
       "((i == 15) || (i == 20))")))

(test and
  (is (equal
       (c `(and (> i 0)
                (< i 30)))
       "((i > 0) && (i < 30))")))

(test include
  (is (equal
       (c `(include "stdio.h"))
       "#include <stdio.h>")))

(test defun
  (is (equal
       (c `(defun (main :int) ((argc :int) (argv :char**))
             (@printf (str "Hello, world!"))
             (return 0)))
       "int main (int argc, char **argv) {
  printf(\"Hello, world!\");
  return 0;
}"))
  (is (equal
       (c `(defun (main :int) ()
             (set (x :int) 5)
             (set (y :int) 10)
             (return 0)))
       "int main () {
  int x = 5;
  int y = 10;
  return 0 ;
}")))

(test assignment
  (is (equal
       (c `(set (x :int) 5))
       "int x = 5"))
  (is (equal
       (c `(set (y :int) 10))
       "int y = 10")))

(test printf
  (is (equal
       (c `(@printf (str "x = %d, y = %d \\n") x y))
       "printf(\"x = %d, y = %d\\n\", x, y)")))

(test return
  (is (equal
       (c `(return 0))
       "return 0")))
