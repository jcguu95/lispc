(include :system ("stdio.h"))

(defun (main :int) ()
  (declare () (i :int))
  (declare () (j :int))                    ; TODO Can I make multiple declarations in one line?
  (@printf (str "Enter an integer for i: "))
  (@scanf (str "%d") (& i))
  (case i
    (1
     (@printf (str "i = 1\\n")))
    (2
     (@printf (str "i = 2\\n")))
    (3
     (@printf (str "i = 3\\n"))
     (@printf (str "Enter an integer for j: "))
     (@scanf  (str "%d") (& j))
     (case j
       (1 (@printf (str "j = 1\\n")))
       (t (@printf (str "Wrong guess. Aborting..\\n")))))
    (t (@printf (str "Wrong guess. Aborting..\\n"))))
  (return 0))
