;; #include <stdio.h>
;; #include <stdlib.h>
(include :system ("stdio.h" "stdlib.h"))

;; typedef struct X X;
(deftype (:struct X) X)

;; struct X {
;;   int value;
;;   X  *next;
;; };
(defstruct X
  ((value :int)
   (next  :X*)))

;; int main() {
;;   X *x1 = malloc(sizeof(X));
;;   x1->value = 10;
;;   X *x2 = malloc(sizeof(X));
;;   x2->value = 20;
;;   x1->next = x2;
;;   X *x3 = malloc(sizeof(X));
;;   x3->value = 30;
;;   x2->next = x3;
;;   printf("x1->value = %d\n", x1->value);
;;   printf("x1->next->value = %d\n", x1->next->value);
;;   printf("x1->next->next->value = %d\n", x1->next->next->value);
;;   return 0;
;; }
(defun (main :int) ()
  (declare (x1 :X*) (@malloc (@sizeof X)))
  (declare (x2 :X*) (@malloc (@sizeof X)))
  (declare (x3 :X*) (@malloc (@sizeof X)))
  (set (-> x1 value) 10)
  (set (-> x2 value) 20)
  (set (-> x3 value) 30)
  (set (-> x1 next)  x2)
  (set (-> x2 next)  x3)
  (@printf (str "x1->value             = %d\\n") (-> x1 value))
  (@printf (str "x1->next->value       = %d\\n") (-> x1 next value))
  (@printf (str "x1->next->next->value = %d\\n") (-> x1 next next value))
  (return 0))
