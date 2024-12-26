(include :system ("stdio.h"))

(defun (main :int) ()
  (declare (x :int))
  (declare (y :int))
  (@printf (str "Enter an integer for x:\\n"))
  (@scanf (str "%d") (& x))
  (@printf (str "Enter an integer for y:\\n"))
  (@scanf (str "%d") (& y))
  (@printf (str "You entered x = %d and y = %d\\n") x y)
  (block
      (cond ((< x 0)
             (goto negative))
            (t
             (@printf (str "x is positive.\\n"))))
    (cond ((< y 0)
           (@printf (str "y is negative.\\n"))
           (block
               (label negative)
             (cond ((< x 0)
                    (@printf (str "x is negative.\\n")))))
           (return 0)))))
