(include :system ("stdio.h"))

(defun (main :int) ()
  ;; Stage 0
  (lisp
   (defun multi-for (bindings body)
     (loop :for binding :in (reverse bindings)
           :do (setf body `((for ((declare () (,(nth 0 binding) :int) 0)
                                  (< ,(nth 0 binding) ,(nth 1 binding))
                                  (++ ,(nth 0 binding)))
                             ,@body))))
     (car body))
   (multi-for '((i 3) (j 2) (k 2))
              '((@printf (str "%d ") (+ i j k))
                (@printf (str "%d\\n") i))))
  (return 0))

;;; Stage 1
;;
;; (FOR ((DECLARE () (I :INT) 0) (< I 3) (++ I))
;;  (FOR ((DECLARE () (J :INT) 0) (< J 2) (++ J))
;;   (FOR ((DECLARE () (K :INT) 0) (< K 2) (++ K))
;;        (@PRINTF (STR "%d ") (+ I J K))
;;        (@PRINTF (STR "%d\\n") I))))

;;; Stage 2
;;
;; for (int i = 0; ((i) < (3)); ((i)++)) {
;;   for (int j = 0; ((j) < (2)); ((j)++)) {
;;     for (int k = 0; ((k) < (2)); ((k)++)) {
;;       printf("%d ", ((i) + (j) + (k)));
;;       printf("%d\n", i);
;;     }
;;   }
;; }
