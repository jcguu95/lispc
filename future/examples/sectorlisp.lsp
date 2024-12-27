;; Copyright 2020 Justine Alexandra Roberts Tunney
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;; SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
;; IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;; #include "bestline.h"
;; #include <ctype.h>
;; #include <stdio.h>
;; #include <stdlib.h>
;; #include <string.h>
;; #include <locale.h>
;; #include <limits.h>
(include :system ("ctype.h" "stdio.h" "stdlib.h" "string.h" "locale.h" "limits.h")
         :local ("bestline.h"))

;; /*───────────────────────────────────────────────────────────────────────────│─╗
;; │ The LISP Challenge § LISP Machine                                        ─╬─│┼
;; ╚────────────────────────────────────────────────────────────────────────────│*/

;; #define kT          4
;; #define kQuote      6
;; #define kCond       12
;; #define kRead       17
;; #define kPrint      22
;; #define kAtom       28
;; #define kCar        33
;; #define kCdr        37
;; #define kCons       41
;; #define kEq         46
;; #define M (RAM + sizeof(RAM) / sizeof(RAM[0]) / 2)
;; #define S "NIL\0T\0QUOTE\0COND\0READ\0PRINT\0ATOM\0CAR\0CDR\0CONS\0EQ"
(#define
  |kT| 4
  |kQuote| 6
  |kCond| 12
  |kRead| 17
  |kPrint| 22
  |kAtom| 28
  |kCar| 33
  |kCdr| 37
  |kCons| 41
  |kEq| 46
  |m| (+ |ram| (/ (@sizeof |ram|) (@sizeof (@ |ram| 0)) 2))
  |s| (str "NIL\\0T\\0QUOTE\\0COND\\0READ\\0PRINT\\0ATOM\\0CAR\\0CDR\\0CONS\\0EQ"))

;; int cx; /* stores negative memory use */
;; int dx; /* stores lookahead character */
;; int RAM[0100000]; /* your own ibm7090 */
(declare (cx :int)
         (dx :int)
         (|ram| (:array 0100000 :int)))

;; Intern() {
;;   int i, j, x;
;;   for (i = 0; (x = M[i++]);) {
;;     for (j = 0;; ++j) {
;;       if (x != RAM[j]) break;
;;       if (!x) return i - j - 1;
;;       x = M[i++];
;;     }
;;     while (x)
;;       x = M[i++];
;;   }
;;   j = 0;
;;   x = --i;
;;   while ((M[i++] = RAM[j++]));
;;   return x;
;; }
(defun (intern :int) ()
  (declare (i :int)
           (j :int)
           (x :int))
  (for ((set i 0) (set x (@ |m| (++ i))) ())
       (for ((set j 0) () (++ j))
            (cond ((!= x (@ |ram| j))
                   (break)))
            (cond ((not x)
                   (return (- i j 1))))
            (set x (@ |m| (++ i))))
       (while x
              (set x (@ |m| (++ i)))))
  (set j 0)
  (set x (-- i))
  (while (set (@ |m| (++ i))
              (@ |ram| (++ j))))
  (return x))

;; GetChar() {
;;   int c, t;
;;   static char *l, *p;
;;   if (l || (l = p = bestlineWithHistory("* ", "sectorlisp"))) {
;;     if (*p) {
;;       c = *p++ & 255;
;;     } else {
;;       free(l);
;;       l = p = 0;
;;       c = '\n';
;;     }
;;     t = dx;
;;     dx = c;
;;     return t;
;;   } else {
;;     PrintChar('\n');
;;     exit(0);
;;   }
;; }
(defun (getchar :int) ()
  (declare (c :int)
           (t :int)
           (l (:pointer :s-char))
           (p (:pointer :s-char)))
  (cond ((or l (set-multi l p (@|bestlineWithHistory| (str "* " "sectorlisp"))))
         (cond ((* p)
                (set c (& (* (++ p)) 255)))
               ("TODO"))
         ))
  )
