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

(include :system ("stdio.h"))

(defun (main :int) ()
  (declare (i :int) 20)
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
