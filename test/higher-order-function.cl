;; #include <stdio.h>
;; #include <stdlib.h>
(headers stdio
         stdlib)

;; int square(int x) { return x * x; }
(func square int ((x int))
      (return (* x x)))

;; int* map(int (*f)(int), int* arr, size_t size) {
;;     int* result = (int*)malloc(size * sizeof(int));
;;     for (size_t i = 0; i < size; i++) { result[i] = f(arr[i]); }
;;     return result;
;; }
(func (pt map) int (("(*f)(int)" int)     ; TODO Dirty
                    ((pt arr) int)
                    (size size-t))
      (var (pt result) int (cast (@malloc (* size (@sizeof int))) (typ* int)))
      (for (var i size-t 0) (< i size) (++ i)
           (set ([]result i) (@f ([]arr i))))
      (return result))

;; int main() {
;;     int numbers[] = {1, 2, 3, 4, 5};
;;     size_t size = sizeof(numbers) / sizeof(numbers[0]);
;;     int* result = map(square, numbers, size);
;;     for (size_t i = 0; i < size; i++) { printf("%d ", result[i]); }
;;     printf("\n");
;;     return 0;
;; }
(func main int ()
      (var numbers[] int ({}s 1 2 3 4 5)) ; TODO Dirty
      (var size size-t (/ (@sizeof numbers) (@sizeof ([]numbers 0))))
      (var (pt result) int (@map square numbers size))
      (for (var i size-t 0) (< i size) (++ i)
           (@printf (str "%d ") ([]result i)))
      (@printf (str "\\n"))
      (return 0))
