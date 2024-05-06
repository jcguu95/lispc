;; #define SWAP(a, b) do { typeof(a) temp = a; a = b; b = temp; } while (0)
;; int main() {
;;     int x = 5, y = 10;
;;     SWAP(x, y);
;;     printf("x = %d, y = %d\n", x, y);
;;     return 0;
;; }

(let ((swap (x y)
            (do-while 0
              (def (temp (:type-of a)) a)
              (def a b)
              (def b temp))))
  (defun (main :int) ()
    (set (x :int) 5)
    (set (y :int) 10)
    (swap x y)
    (@printf (str "x = %d, y = %d\\n") x y)
    (return 0)))

;;; TESTs

(c `(set (x :int) 5))
"int x = 5;"

(c `(set (y :int) 10))
"int y = 10;"

(c `(@printf (str "x = %d, y = %d \\n") x y))
"printf(\"x = %d, y = %d\\n\", x, y);"

(c `(return 0))
"return 0;"

(c `(defun (main :int)
      (set (x :int) 5)
      (set (y :int) 10)
      (return 0)))
"int main () {
  int x = 5;
  int y = 10;
  return 0 ;
}"
