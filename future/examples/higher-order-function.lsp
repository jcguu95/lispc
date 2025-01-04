;; #include <stdio.h>
;; #include <stdlib.h>
(include :system ("stdio.h" "stdlib.h"))

;; int square(int x) { return x * x; }
(defun (square :int) ((x :int))
  (return (* x x)))

;; int *map(int (*f)(int), int *arr, size_t size) {
;;     int *result = (int*)malloc(size * sizeof(int));
;;     for (size_t i = 0; i < size; i++) { result[i] = f(arr[i]); }
;;     return result;
;; }

;; FIXME Adopt new type spec for function declaration type.
(defun (map (:pointer :int)) ((f    (:function :int (:int)))
                              (arr  (:pointer :int))
                              (size :size-t))
  (declare () (result (:pointer :int))
           (cast (:pointer :int) (@malloc (* size (@sizeof :int)))))
  (for ((declare () (i :size-t) 0)
        (< i size)
        (++ i))
       (set (@ result i) (@f (@ arr i))))
  (return result))

;; int main() {
;;     int numbers[] = {1, 2, 3, 4, 5};
;;     size_t size = sizeof(numbers) / sizeof(numbers[0]);
;;     int* result = map(square, numbers, size);
;;     for (size_t i = 0; i < size; i++) { printf("%d ", result[i+1]); printf("%d ", result[i]); }
;;     printf("\n");
;;     return 0;
;; }

(defun (main :int) ()
  (declare () (numbers (:array () :int)) (vec 1 2 3 4 5 6 7 8 9 10))
  (declare () (size :size-t) (/ (@sizeof numbers)
                             (@sizeof (@ numbers 0))))
  (declare () (result (:pointer :int))
           (@map square numbers size))
  (for ((declare () (i :size-t) 0)
        (< i size)
        (set i (+ i 2)))
       (@printf (str "%d ") (@ result i))
       (@printf (str "%d ") (@ result (+ i 1))))
  (return 0))
