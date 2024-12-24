(block
  (cond ((< x 0)
         (goto negative)))
  (cond ((< y 0)
         (block
           (label negative)
           (@printf (str "Negative\\n"))
           (return)))))
