#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <limits.h>

#define kT 4
#define kQuote 6
#define kCond 12
#define kRead 17
#define kPrint 22
#define kAtom 28
#define kCar 33
#define kCdr 37
#define kCons 41
#define kEq 46
#define M ((RAM) + (((sizeof(RAM)) / (sizeof(RAM[0])) / (2))))
#define S "NIL\0T\0QUOTE\0COND\0READ\0PRINT\0ATOM\0CAR\0CDR\0CONS\0EQ"


int cx;
int dx;
int (RAM)[100000];

int (addlist) (int);
int (getobject) (int);
int (cons) (int,int);
int (car) (int);
int (cdr) (int);
int (printobject) (int);
int (eval) (int,int);

int intern () {
  int i;
  int j;
  int x;
  for (i = 0; x = M[((i)++)]; ) {
    for (j = 0; ; ((j)++)) {
      if (((x) != (RAM[j]))) {
        break;
      }
      if ((!(x))) {
        return (((i) - (j) - (1)));
      }
      x = M[((i)++)];
    }
    while (x) {
      x = M[((i)++)];
    }
  }
  j = 0;
  x = ((i)--);
  while (M[((i)++)] = RAM[((j)++)]) {

  }
  return (x);
}

char *(GetLine) (const char *(prompt)) {
  static char (buffer)[1024];
  printf("%s", prompt);
  fflush(stdout);
  if (fgets(buffer, sizeof(buffer), stdin)) {
    size_t len = strlen(buffer);
    if (((((len) > (0))) && (((buffer[((len) - (1))]) == ('\n'))))) {
      buffer[((len) - (1))] = '\0';
    }
    return (buffer);
  }
  return (NULL);
}

void printchar (int b) {
  fputc(b, stdout);
}

int getchar () {
  static char *(line) = NULL;
  static char *(ptr) = NULL;
  if ((((!(line))) || ((!(((ptr))))))) {
    line = GetLine("");
    if ((!(line))) {
      exit(0);
    }
    ptr = line;
  }
  int c = *((ptr)++);
  int t = dx;
  dx = c;
  return (t);
}

int gettoken () {
  int c;
  int i = 0;
  do {
    if (((c = getchar()) > (' '))) {
      RAM[((i)++)] = c;
    }
  } while (((((((c) <= (' '))) || (((c) > (')'))))) && (((dx) > (')')))));
  RAM[i] = 0;
  return (c);
}

int getlist () {
  int c = gettoken();
  if (((c) == (')'))) {
    return (0);
  }
  return (addlist(getobject(c)));
}

int getobject (int c) {
  if (((c) == ('('))) {
    return (getlist());
  }
  return (intern());
}

int addlist (int x) {
  return (cons(x, getlist()));
}

int read () {
  return (getobject(gettoken()));
}

int printatom (int x) {
  int c;
  for (; ; ) {
    if ((!(c = M[((x)++)]))) {
      break;
    }
    printchar(c);
  }
}

int printlist (int x) {
  printchar('(');
  printobject(car(x));
  while (x = cdr(x)) {
    if (((x) < (0))) {
      printchar(' ');
      printobject(car(x));
    } else {
      printchar(L'∙');
      printobject(x);
      break;
    }
  }
  printchar(')');
}

int printobject (int x) {
  if (((x) < (0))) {
    printlist(x);
  } else {
    printatom(x);
  }
}

int print (int e) {
  printobject(e);
}

int printnewline () {
  printchar('\n');
}

int car (int x) {
  return (M[x]);
}

int cdr (int x) {
  return (M[((x) + (1))]);
}

int cons (int car, int cdr) {
  M[((cx)--)] = cdr;
  M[((cx)--)] = car;
  return (cx);
}

int gc (int x, int m, int k) {
  if (((x) < (m))) {
    return (((cons(gc(car(x), m, k), gc(cdr(x), m, k))) + (k)));
  }
  return (x);
}

int evlis (int m, int a) {
  if ((!(m))) {
    return (0);
  }
  int x = eval(car(m), a);
  return (cons(x, evlis(cdr(m), a)));
}

int pairlis (int x, int y, int a) {
  if ((!(x))) {
    return (a);
  }
  return (cons(cons(car(x), car(y)), pairlis(cdr(x), cdr(y), a)));
}

int assoc (int x, int y) {
  if ((!(y))) {
    return (0);
  } else if ((x) == (car(car(y)))) {
    return (cdr(car(y)));
  }
  return (assoc(x, cdr(y)));
}

int evcon (int c, int a) {
  if (eval(car(car(c)), a)) {
    return (eval(car(cdr(car(c))), a));
  } else {
    return (evcon(cdr(c), a));
  }
}

int apply (int f, int x, int a) {
  if (((f) < (0))) {
    return (eval(car(cdr(cdr(f))), pairlis(car(cdr(f)), x, a)));
  } else if ((f) > (kEq)) {
    return (apply(eval(f, a), x, a));
  } else if ((f) == (kEq)) {
    return (((((car(x)) == (car(cdr(x))))) ? (kT) : (0)));
  } else if ((f) == (kCons)) {
    return (cons(car(x), car(cdr(x))));
  } else if ((f) == (kAtom)) {
    return (((((car(x)) < (0))) ? (0) : (kT)));
  } else if ((f) == (kCar)) {
    return (car(car(x)));
  } else if ((f) == (kCdr)) {
    return (cdr(car(x)));
  } else if ((f) == (kRead)) {
    return (read());
  } else if ((f) == (kPrint)) {
    if ((!(x))) {
      printnewline();
    }
    return (print(car(x)));
  }
  return (0);
}

int eval (int e, int a) {
  int A;
  int B;
  int C;
  if (((e) >= (0))) {
    return (assoc(e, a));
  }
  if (((car(e)) == (kQuote))) {
    return (car(cdr(e)));
  }
  A = cx;
  if (((car(e)) == (kCond))) {
    e = evcon(cdr(e), a);
  } else {
    e = apply(car(e), evlis(cdr(e), a), a);
  }
  B = cx;
  e = gc(e, A, ((A) - (B)));
  C = cx;
  while (((C) < (B))) {
    M[((A)--)] = M[((B)--)];
  }
  cx = A;
  return (e);
}

int main () {
  int i;
  setlocale(LC_ALL, "");
  for (i = 0; ((i) < (sizeof(S))); ((i)++)) {
    M[i] = S[i];
  }
  for (; ; ) {
    cx = 0;
    print(eval(read(), 0));
    printnewline();
  }
}


