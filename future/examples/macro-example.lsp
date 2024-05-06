;;; STAGE 0
(lisp
 (defun gen-foo (type)
   `(defun (,(format nil "foo-~a" type) ((x ,type) (y ,type)))
        (return (* 2 (+ x y)))))
 (loop :for type :in '(:int :float :double)
       :collect (gen-foo type)))

;;; STAGE 1
;;
;; (defun (foo-int :int) ((x :int) (y :int))
;;   (return (* 2 (+ x y))))
;;
;; (defun ("foo-float" :float) ((x :float) (y :float))
;;   (return (* 2 (+ x y))))
;;
;; (defun (foo-double :double) ((x :double) (y :double))
;;   (return (* 2 (+ x y))))

;;; STAGE 2
;; int foo_INT(int x, int y) {
;;   return x + y * 2;
;; }
;;
;; float foo_FLOAT(float x, float y) {
;;   return x + y * 2;
;; }
;;
;; double foo_DOUBLE(double x, double y) {
;;   return x + y * 2;
;; }
