;;; Stage 0
(lisp
 (defun multi-for (bindings body)
   (loop :for binding :in (reverse bindings)
         :do (setf body `(for ((def (,(nth 0 binding) :int) 0)
                               (< ,(nth 0 binding) ,(nth 1 binding))
                               (++ ,(nth 0 binding)))
                           ,body)))
   body)
 (multi-for '((i 3) (j 3) (k 3) (l 4) (m 5))
            '((@printf (str "%d %d %d %d %d") i j k l m)
              (@printf (str "%d %d %d %d %d") m l k j i))))

;;; Stage 1
;;
;; (for ((def (i :int) 0) (< i 3) (++ i))
;;   (for ((def (j :int) 0) (< j 3) (++ j))
;;     (for ((def (k :int) 0) (< k 3) (++ k))
;;       (for ((def (l :int) 0) (< l 4) (++ l))
;;         (for ((def (m :int) 0) (< m 5) (++ m))
;;           (@printf (str "%d %d %d %d %d") i j k l m)
;;           (@printf (str "%d %d %d %d %d") m l k j i))))))

;;; Stage 2
;;
;; for(int i=0;((i)<(3));++(i)) {
;;     for(int j=0;((j)<(3));++(j)) {
;;         for(int k=0;((k)<(3));++(k)) {
;;             for(int l=0;((l)<(4));++(l)) {
;;                 for(int m=0;((m)<(5));++(m)) {
;;                     printf("%d %d %d %d %d",i,j,k,l,m);
;;                     printf("%d %d %d %d %d",m,l,k,j,i);
;;                 };
;;             };
;;         };
;;     };
;;  }
