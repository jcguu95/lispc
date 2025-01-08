# PAREN/C

`PAREN/C` is a customizable lispy language that compiles to `C`.

``` common-lisp
(include :system ("stdio.h"))

(defun (main :int) ((argc :int) (argv :char**))
  (@printf (str "Hello, world!"))
  (return 0))
  
;; #include <stdio.h>
;;
;; int main(int argc,char **argv)
;; {
;;    printf("Hello, world!");
;;    return 0;
;; }
```
The `./examples` directory contains several nontrivial examples. A standout is
the full [SectorLisp](https://justine.lol/sectorlisp/) clone
(`./examples/sectorlisp.lsp` and `./examples/sectorlisp.c`), a turing-complete
lisp programming language designed to fit within the master boot sector. All
examples in the `./examples` directory are unit tested in
`./test/paren.test.lisp`. For instance, a test suite demonstrates how
`sectorlisp.lsp` evaluates the metacircular evaluator. Additional examples
will be added in the future.

## Usage

The main function `c` compiles lisp forms into `c` code. 

``` common-lisp
CL-USER> (asdf:test-system "paren" :force t) ; loads and tests the system
CL-USER> (in-package :paren)
PAREN> (c '(defun (main :int) ((argc :int) (argv :char**))
             (@printf (str "Hello, world!"))
             (return 0))) ;=>
"int main(int argc,char **argv)
{
    printf("Hello, world!");
    return 0;
}"
```

Interoperability through the `LISP` operator enables you to write `paren/c`
code within `lisp`.

``` common-lisp
;; ./examples/nested-loops.lsp
(include :system ("stdio.h"))

(defun (main :int) ()
  ;; Stage 0 of compilation
  (LISP
   ;; A Common Lisp function that generates LSP code as LISP lists.
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

;;; Stage 1 of compilation (LSP->LSP)
;; (FOR ((DECLARE () (I :INT) 0) (< I 3) (++ I))
;;  (FOR ((DECLARE () (J :INT) 0) (< J 2) (++ J))
;;   (FOR ((DECLARE () (K :INT) 0) (< K 2) (++ K))
;;        (@PRINTF (STR "%d ") (+ I J K))
;;        (@PRINTF (STR "%d\\n") I))))

;;; Stage 2 of compilation (LSP -> C)
;; for (int i = 0; ((i) < (3)); ((i)++)) {
;;   for (int j = 0; ((j) < (2)); ((j)++)) {
;;     for (int k = 0; ((k) < (2)); ((k)++)) {
;;       printf("%d ", ((i) + (j) + (k)));
;;       printf("%d\n", i);
;;     }
;;   }
;; }
```

## State of the Project

We are currently refining the language by porting additional `C` code into
`PAREN/C`. It is not ready for use yet. Contributions are welcome (see
`TODO.md` for details).
