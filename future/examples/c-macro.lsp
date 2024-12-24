;; #define EQUAL?(a,b) ((a) == (b))
;;
;; #undef EQUAL?
;;
;; #define SWAP(a,b) do \
;;   { typeof(a) temp = a; \
;;     a = b; \
;;     b = temp; \
;;   } while (0)
;;
;; #undef SWAP

;; #define EQUAL?(a,b) ((a) == (b))
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
;; #undef EQUAL?
;; #undef SWAP

(defmacro |equal?| (a b) (== a b))

(undefmacro |equal?|)

(defmacro |swap| (a b)
  (do-while 0
    (declare (temp (:type-of a)) a)
    (set a b)
    (set b temp)))

(undefmacro |swap|)

(with-c-macro
    ((|equal?| (a b) (== a b))
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
