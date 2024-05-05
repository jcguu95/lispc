;; #include <stdio.h>
;; #include <stdlib.h>
(include "stdio.h")
(include "stdlib.h")

;; typedef struct X X;
(typedef (struct X) X)

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
  (def (x1 :X*) (@malloc (@sizeof X)))
  (def (x2 :X*) (@malloc (@sizeof X)))
  (def (x3 :X*) (@malloc (@sizeof X)))
  (def (-> x1 value) 10)
  (def (-> x2 value) 20)
  (def (-> x3 value) 30)
  (def (-> x1 next)  x2)
  (def (-> x2 next)  x3)
  (@printf (str "x1->value             = %d\\n") (-> x1 value))
  (@printf (str "x1->next->value       = %d\\n") (-> x1 next value))
  (@printf (str "x1->next->next->value = %d\\n") (-> x1 next next value))
  (return 0))
