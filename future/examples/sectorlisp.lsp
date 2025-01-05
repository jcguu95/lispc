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

;; #include <ctype.h>
;; #include <stdio.h>
;; #include <stdlib.h>
;; #include <string.h>
;; #include <locale.h>
;; #include <limits.h>
(include :system ("ctype.h" "stdio.h" "stdlib.h" "string.h" "locale.h" "limits.h"))

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
(define
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
(declare ()
         (cx :int) ()
         (dx :int) ()
         (|ram| (:array 0100000 :int)) ())

;; int AddList(int x);
;; int GetObject(int c);
;; int Cons(int car, int cdr);
;; int Car(int x);
;; int Cdr(int x);
;; int PrintObject(int x);
;; int Eval(int e, int a);

 (declare ()
          (addlist (:function :int (:int))) ()
          (getobject (:function :int (:int))) ()
          (cons (:function :int (:int :int))) ()
          (car (:function :int (:int))) ()
          (cdr (:function :int (:int))) ()
          (printobject (:function :int (:int))) ()
          (eval (:function :int (:int :int))) ())

;; int Intern() {
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
  (declare ()
           (i :int) ()
           (j :int) ()
           (x :int) ())
  (for ((set i 0)
        (set x (@ |m| (++ i)))
        ())
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

;; char *GetLine(const char *prompt) {
;;     static char buffer[1024];
;;     printf("%s", prompt);
;;     fflush(stdout);
;;     if (fgets(buffer, sizeof(buffer), stdin)) {
;;         // Strip newline character
;;         size_t len = strlen(buffer);
;;         if (len > 0 && buffer[len - 1] == '\n') {
;;             buffer[len - 1] = '\0';
;;         }
;;         return buffer;
;;     }
;;     return NULL;
;; }
(defun (|GetLine| (:pointer :char)) ((prompt (:pointer :c-char)))
  (declare :static (buffer (:array 1024 :char)))
  (@printf (str "%s") prompt)
  (@fflush stdout)
  (cond ((@fgets buffer (@sizeof buffer) stdin)
         (declare () (len :size-t) (@strlen buffer))
         (cond ((and (> len 0)
                     (== (@ buffer (- len 1)) (char "\\n")))
                (set (@ buffer (- len 1)) (char "\\0"))))
         (return buffer)))
  (return "NULL"))

;; void PrintChar(int b) {
;;     fputc(b, stdout);
;; }
(defun (printchar :void) ((b :int))
  (@fputc b stdout))

;; int GetChar() {
;;     static char *line = NULL;
;;     static char *ptr = NULL;

;;     if (!line || !*ptr) {
;;         // line = GetLine("* ");
;;         line = GetLine("");
;;         if (!line) {
;;             // PrintChar('\n');
;;             exit(0);
;;         }
;;         ptr = line;
;;     }

;;     int c = *ptr++;
;;     int t = dx;
;;     dx = c;
;;     return t;
;; }
(defun (getchar :int) ()
  (declare :static
           (line (:pointer :char)) "NULL"
           (ptr  (:pointer :char)) "NULL")
  (cond ((or (not line)
             (not (* ptr)))
         (set line (@|GetLine| (str "")))
         (cond ((not line)
                (exit 0)))
         (set ptr line)))
  (declare ()
           (c :int) (deref (++ ptr))
           (t :int) dx)
  (set dx c)
  (return t))

;; int GetToken() {
;;   int c, i = 0;
;;   do if ((c = GetChar()) > ' ') RAM[i++] = c;
;;   while (c <= ' ' || (c > ')' && dx > ')'));
;;   RAM[i] = 0;
;;   return c;
;; }
(defun (gettoken :int) ()
  (declare ()
           (c :int) ()
           (i :int) 0)
  (do-while (and (or (<= c (char " "))
                     (>  c (char ")")))
                 (> dx (char ")")))
    (cond ((> (set c (@getchar))
              (char " "))
           (set (@ |ram| (++ i)) c))))
  (set (@ |ram| i) 0)
  (return c))

;; int GetList() {
;;   int c = GetToken();
;;   if (c == ')') return 0;
;;   return AddList(GetObject(c));
;; }
(defun (getlist :int) ()
  (declare () (c :int) (@gettoken))
  (cond ((== c (char ")"))
         (return 0)))
  (return (@addlist (@getobject c))))

;; int GetObject(c) {
;;   if (c == '(') return GetList();
;;   return Intern();
;; }
(defun (getobject :int) ((c :int))
  (cond ((== c (char "("))
         (return (@getlist))))
  (return (@intern)))

;; int AddList(x) {
;;   return Cons(x, GetList());
;; }
(defun (addlist :int) ((x :int))
  (return (@cons x (@getlist))))

;; int Read() {
;;   return GetObject(GetToken());
;; }
(defun (read :int) ()
  (return (@getobject (@gettoken))))

;; int PrintAtom(x) {
;;   int c;
;;   for (;;) {
;;     if (!(c = M[x++])) break;
;;     PrintChar(c);
;;   }
;; }
(defun (printatom :int) ((x :int))
  (declare () (c :int))
  (for (() () ())
       (cond ((not (set c (@ |m| (++ x))))
              (break)))
       (@printchar c)))

;; int PrintList(x) {
;;   PrintChar('(');
;;   PrintObject(Car(x));
;;   while ((x = Cdr(x))) {
;;     if (x < 0) {
;;       PrintChar(' ');
;;       PrintObject(Car(x));
;;     } else {
;;       PrintChar(L'∙');
;;       PrintObject(x);
;;       break;
;;     }
;;   }
;;   PrintChar(')');
;; }
(defun (printlist :int) ((x :int))
  (@printchar (char "("))
  (@printobject (@car x))
  (while (set x (@cdr x))
         (cond ((< x 0)
                (@printchar (char " "))
                (@printobject (@car x)))
               (t
                (@printchar (wide-char "∙"))
                (@printobject x)
                (break))))
  (@printchar (char ")")))

;; int PrintObject(int x) {
;;   if (x < 0) {
;;     PrintList(x);
;;   } else {
;;     PrintAtom(x);
;;   }
;; }
(defun (printobject :int) ((x :int))
  (cond ((< x 0) (@printlist x))
        (t (@printatom x))))

;; int Print(e) {
;;   PrintObject(e);
;; }
(defun (print :int) ((e :int))
  (@printobject e))

;; int PrintNewLine() {
;;   PrintChar('\n');
;; }
(defun (printnewline :int) ()
  (@printchar (char "\\n")))

;; /*───────────────────────────────────────────────────────────────────────────│─╗
;; │ The LISP Challenge § Bootstrap John McCarthy's Metacircular Evaluator    ─╬─│┼
;; ╚────────────────────────────────────────────────────────────────────────────│*/

;; int Car(int x) {
;;   return M[x];
;; }
(defun (car :int) ((x :int))
  (return (@ |m| x)))

;; int Cdr(int x) {
;;   return M[x + 1];
;; }
(defun (cdr :int) ((x :int))
  (return (@ |m| (+ x 1))))

;; int Cons(car, cdr) {
;;   M[--cx] = cdr;
;;   M[--cx] = car;
;;   return cx;
;; }
(defun (cons :int) ((car :int) (cdr :int))
  (set (@ |m| (-- cx)) cdr)
  (set (@ |m| (-- cx)) car)
  (return cx))

;; int Gc(x, m, k) {
;;   return x < m ? Cons(Gc(Car(x), m, k),
;;                       Gc(Cdr(x), m, k)) + k : x;
;; }
(defun (gc :int) ((x :int) (m :int) (k :int))
  (cond ((< x m)
         (return (+ (@cons (@gc (@car x) m k)
                           (@gc (@cdr x) m k))
                    k))))
  (return x))

;; int Evlis(m, a) {
;;   if (m) {
;;     int x = Eval(Car(m), a);
;;     return Cons(x, Evlis(Cdr(m), a));
;;   } else {
;;     return 0;
;;   }
;; }
(defun (evlis :int) ((m :int) (a :int))
  (cond ((not m)
         (return 0)))
  (declare () (x :int) (@eval (@car m) a))
  (return (@cons x (@evlis (@cdr m) a))))

;; int Pairlis(x, y, a) {
;;   return x ? Cons(Cons(Car(x), Car(y)),
;;                   Pairlis(Cdr(x), Cdr(y), a)) : a;
;; }
(defun (pairlis :int) ((x :int) (y :int) (a :int))
  (cond ((not x)
         (return a)))
  (return (@cons (@cons (@car x) (@car y))
                 (@pairlis (@cdr x) (@cdr y) a))))

;; int Assoc(x, y) {
;;   if (!y) return 0;
;;   if (x == Car(Car(y))) return Cdr(Car(y));
;;   return Assoc(x, Cdr(y));
;; }
(defun (assoc :int) ((x :int) (y :int))
  (cond ((not y)
         (return 0))
        ((== x (@car (@car y)))
         (return (@cdr (@car y)))))
  (return (@assoc x (@cdr y))))

;; int Evcon(c, a) {
;;   if (Eval(Car(Car(c)), a)) {
;;     return Eval(Car(Cdr(Car(c))), a);
;;   } else {
;;     return Evcon(Cdr(c), a);
;;   }
;; }
(defun (evcon :int) ((c :int) (a :int))
  (cond ((@eval (@car (@car c)) a)
         (return (@eval (@car (@cdr (@car c))) a)))
        (t
         (return (@evcon (@cdr c) a)))))

;; int Apply(f, x, a) {
;;   if (f < 0)       return Eval(Car(Cdr(Cdr(f))), Pairlis(Car(Cdr(f)), x, a));
;;   if (f > kEq)     return Apply(Eval(f, a), x, a);
;;   if (f == kEq)    return Car(x) == Car(Cdr(x)) ? kT : 0;
;;   if (f == kCons)  return Cons(Car(x), Car(Cdr(x)));
;;   if (f == kAtom)  return Car(x) < 0 ? 0 : kT;
;;   if (f == kCar)   return Car(Car(x));
;;   if (f == kCdr)   return Cdr(Car(x));
;;   if (f == kRead)  return Read();
;;   if (f == kPrint) return (x ? Print(Car(x)) : PrintNewLine()), 0;
;; }
(defun (apply :int) ((f :int) (x :int) (a :int))
  (cond ((< f 0)
         (return (@eval (@car (@cdr (@cdr f)))
                        (@pairlis (@car (@cdr f)) x a))))
        ((> f |kEq|)
         (return (@apply (@eval f a) x a)))
        ((== f |kEq|)
         (return (? (== (@car x) (@car (@cdr x)))
                    |kT|
                    0)))
        ((== f |kCons|)
         (return (@cons (@car x) (@car (@cdr x)))))
        ((== f |kAtom|)
         (return (? (< (@car x) 0)
                    0
                    |kT|)))
        ((== f |kCar|)
         (return (@car (@car x))))
        ((== f |kCdr|)
         (return (@cdr (@car x))))
        ((== f |kRead|)
         (return (@read)))
        ((== f |kPrint|)
         (cond ((not x)
                (@printnewline)))
         (return (@print (@car x)))))
  (return 0))

;; int Eval(int e, int a) {
;;   int A, B, C;
;;   if (e >= 0)
;;     return Assoc(e, a);
;;   if (Car(e) == kQuote)
;;     return Car(Cdr(e));
;;   A = cx;
;;   if (Car(e) == kCond) {
;;     e = Evcon(Cdr(e), a);
;;   } else {
;;     e = Apply(Car(e), Evlis(Cdr(e), a), a);
;;   }
;;   B = cx;
;;   e = Gc(e, A, A - B);
;;   C = cx;
;;   while (C < B)
;;     M[--A] = M[--B];
;;   cx = A;
;;   return e;
;; }
(defun (eval :int) ((e :int) (a :int))
  (declare () (|a| :int))
  (declare () (|b| :int))
  (declare () (|c| :int))
  (cond ((>= e 0)
         (return (@assoc e a))))
  (cond ((== (@car e) |kQuote|)
         (return (@car (@cdr e)))))
  (set |a| cx)
  (cond ((== (@car e) |kCond|)
         (set e (@evcon (@cdr e) a)))
        (t
         (set e (@apply (@car e)
                        (@evlis (@cdr e) a)
                        a))))
  (set |b| cx)
  (set e (@gc e |a| (- |a| |b|)))
  (set |c| cx)
  (while (< |c| |b|)
         (set (@ |m| (-- |a|)) (@ |m| (-- |b|))))
  (set cx |a|)
  (return e))

;; /*───────────────────────────────────────────────────────────────────────────│─╗
;; │ The LISP Challenge § User Interface                                      ─╬─│┼
;; ╚────────────────────────────────────────────────────────────────────────────│*/

;; int main() {
;;     int i;
;;     setlocale(LC_ALL, "");

;;     for (i = 0; i < sizeof(S); ++i) M[i] = S[i];
;;     for (;;) {
;;         cx = 0;
;;         Print(Eval(Read(), 0));
;;         PrintNewLine();
;;     }
;; }
(defun (main :int) ()
  (declare () (i :int))
  (@setlocale "LC_ALL" (str ""))
  (for ((set i 0) (< i (@sizeof |s|)) (++ i))
       (set (@ |m| i) (@ |s| i)))
  (for (() () ())
       (set cx 0)
       (@print (@eval (@read) 0))
       (@printnewline)))
