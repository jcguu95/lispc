;; #include <stdio.h>
(include "stdio.h")

;; int main(int argc,char **argv)
;; {
;;    printf("Hello, world!");
;;    return 0;
;; }
(defun (main :int) ((argc :int) (argv :char**))
  (@printf (str "Hello, world!"))
  (return 0))



;;; Tests
(c `(include "stdio.h"))
"#include <stdio.h>"

(c `(defun (main :int) ((argc :int) (argv :char**))
      (@printf (str "Hello, world!"))
      (return 0)))
"int main (int argc, char **argv) {
  printf(\"Hello, world!\");
  return 0;
}"
