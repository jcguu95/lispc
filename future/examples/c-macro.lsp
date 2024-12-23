;; #define EQUALP(a,b) ((a) == (b))
;;
;; #undef EQUALP
;;
;; #define SWAP(a,b) do \
;;   { typeof(a) temp = a; \
;;     a = b; \
;;     b = temp; \
;;   } while (0)
;;
;; #undef SWAP

;; #define EQUALP(a,b) ((a) == (b))
;; #define SWAP(a,b) do \
;;   { typeof(a) temp = a; \
;;     a = b; \
;;     b = temp; \
;;   } while (0)
;;
;; int main() {
;;     int x = 5, y = 10;
;;     SWAP(x, y);
;;     printf("x = %d, y = %d\n", x, y);
;;     return 0;
;; }
;;
;; #undef EQUALP
;; #undef SWAP

(include :system ("stdio.h"))

(defmacro |equalp| (a b) (== a b))
(undefmacro |equalp|)

(defmacro |swap| (a b)
  (do-while 0
    (declare (temp (:type-of a)) a)
    (set a b)
    (set b temp)))
(undefmacro |swap|)

;;; Do everything again, with with-c-macro.
(with-c-macro
    ((|equalp| (a b) (== a b))
     (|swap| (a b)
             (do-while 0
               (declare (temp (:type-of a)) a)
               (set a b)
               (set b temp))))
  (defun (main :int) ()
    (declare (x :int) 5)
    (declare (y :int) 10)
    (|@swap| x y)
    (@printf (str "x = %d, y = %d\\n") x y)
    (return 0)))
