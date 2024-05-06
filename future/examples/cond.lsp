;; #include <stdio.h>

;; int main () {
;;     int i = 20;
;;     if (i == 10) {
;;         printf("i is 10\n");
;;     } else if (i == 15 || i == 20) {
;;         printf("i is 15 or 20\n");
;;     } else if (i > 0 && i < 30) {
;;         printf("i is between 0 and 30\n");
;;     } else {
;;         printf("i is not present\n");
;;     }
;;     return 0;
;; }

(include "stdio.h")

(defun (main :int) ()
  (def (i :int) 20)
  (cond ((== i 10)
         (@printf (str "i is 10\\n")))
        ((or (== i 15)
             (== i 20))
         (@printf (str "i is 15 or 20\\n")))
        ((and (> i 0)
              (< i 30))
         (@printf (str "i is between 0 and 30\\n")))
        (t
         (@printf (str "i is not present\\n"))))
  (return 0))

;;; Tests
(c `(== i 10))
"i == 10"

(c `(or (== i 15)
        (== i 20)))
"(i == 15) || (i == 20)"

(c `(and (> i 0)
         (< i 30)))
"(i > 0) && (i < 30)"
