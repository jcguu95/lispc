;; #include <stdio.h>
;; #include <stdlib.h>
(include stdio)
(include stdlib)

;; int square(int x) { return x * x; }
(defun (square :int) ((x :int))
  (return (* x x)))

;; int *map(int (*f)(int), int *arr, size_t size) {
;;     int *result = (int*)malloc(size * sizeof(int));
;;     for (size_t i = 0; i < size; i++) { result[i] = f(arr[i]); }
;;     return result;
;; }
(defun (map :int*) ((f    :int->int)
                    (arr  :int*)
                    (size :size-t))
  (def (result :int*)
       (cast :int* (@malloc (* size (@sizeof :int)))))
  (for ((def (i :size-t) 0)
        (< i size)
        (++ i))
    (def (@ result i)
         (@f (@ arr i))))
  (return result))

;; int main() {
;;     int numbers[] = {1, 2, 3, 4, 5};
;;     size_t size = sizeof(numbers) / sizeof(numbers[0]);
;;     int* result = map(square, numbers, size);
;;     for (size_t i = 0; i < size; i++) { printf("%d ", result[i]); }
;;     printf("\n");
;;     return 0;
;; }
(defun (main :int) ()
  (def (numbers :int[]) (vec 1 2 3 4 5))
  (def (size :size-t) (/ (@sizeof numbers)
                         (@sizeof (@ numbers 0))))
  (def (result :int*)
       (@map square numbers size))
  (for ((def (i :size-t) 0)
        (< i size)
        (++ i))
    (@printf (str "%d") (@ result i)))
  (@printf (str "\\n"))
  (return 0))
