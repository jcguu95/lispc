(defpackage :paren.test
  (:use :common-lisp
        :paren
        :it.bese.fiveam))

(in-package :paren.test)

(test add-2
 (is (= 2 (add-2 0)))
 (is (= 0 (add-2 -2))))

(test typedef
  (is (equal (c `(deftype (struct X) X))
             "typedef struct X X;"))
  (is (equal (c `(deftype (struct x) x))
             "typedef struct x x;")))

(test defstruct
  (is (equal
       (c `(defstruct X
             ((value :int)
              (next  :X*))))
       "struct X {
  int value;
  X *next;
};")))

(test malloc-size-of
  (is (equal
       (c `(def (x1 :X*) (@malloc (@sizeof X))))
       "X *x1 = malloc(sizeof(X));")))

(test ->
  (is (equal
       (c `(def (-> x1 value) 10))
       "x1->value = 10;"))
  (is (equal
       (c `(def (-> x1 next)  x2))
       "x1->next = x2;"))
  (is (equal
       (c `(@printf (str "Answer: %d.\\n") (-> x1 next next value)))
       "printf(\"Answer: %d.\\n\", x1->next->next->value);")))

(test string
  (is (equal
       (c `(str "ABC"))
       "\"ABC\"")))

(test or
  (is (equal
       (c `(or (== i 15)
               (== i 20)))
       "(i == 15) || (i == 20)")))

(test and
  (is (equal
       (c `(and (> i 0)
                (< i 30)))
       "(i > 0) && (i < 30)")))

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
       (c `(defun (main :int)
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
       "int x = 5;"))
  (is (equal
       (c `(set (y :int) 10))
       "int y = 10;")))

(test printf
  (is (equal
       (c `(@printf (str "x = %d, y = %d \\n") x y))
       "printf(\"x = %d, y = %d\\n\", x, y);")))

(test return
  (is (equal
       (c `(return 0))
       "return 0;")))
