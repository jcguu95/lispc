;; #include <stdio.h>
(include :system ("stdio.h"))

;; Define three similar functions with lisp interop.
;;
;; int foo_INT (int x, int y) {
;;   return (((2) * (((x) + (y)))));
;; };
;; float foo_FLOAT (float x, float y) {
;;   return (((2) * (((x) + (y)))));
;; };
;; double foo_DOUBLE (double x, double y) {
;;   return (((2) * (((x) + (y)))));
;; };
(lisp
 (defun gen-foo (type)
   ;; paren code:
   `(defun (,(intern (format nil "foo-~a" type)) ,type) ((x ,type) (y ,type))
      (return (* 2 (+ x y)))))
 `(compile-each "" ,@(loop :for type :in '(:int :float :double)
                           :collect (gen-foo type))))

(defun (main :int) ()
  (declare (int-x :int) 5)
  (declare (int-y :int) 10)
  (declare (float-x :float) 2.5f)
  (declare (float-y :float) 3.5f)
  (declare (double-x :double) 1.234)
  (declare (double-y :double) 4.567)
  (declare (int-result :int) (@|foo_INT| int-x int-y)) ; FIXME It has to be @|foo-INT|!
  (declare (float-result :float) (@|foo_FLOAT| float-x float-y)) ; FIXME
  (declare (double-result :double) (@|foo_DOUBLE| double-x double-y)) ; FIXME
  (@printf (str "foo_INT(%d, %d) = %d\\n") int-x int-y int-result)
  (@printf (str "foo_FLOAT(%.2f, %.2f) = %.2f\\n") float-x float-y float-result)
  (@printf (str "foo_DOUBLE(%.3f, %.3f) = %.3f\\n") double-x double-y double-result))
