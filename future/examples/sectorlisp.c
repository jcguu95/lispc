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
int (RAM)[0100000];

int (addlist_) (int);
int (getobject_) (int);
int (cons_) (int,int);
int (car_) (int);
int (cdr_) (int);
int (printobject_) (int);
int (eval_) (int,int);

int intern_ () {
  int i;
  int j;
  int x;
  for (i = 0; x = M[((i)++)]; ) {
    for (j = 0; ; (++(j))) {
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
  x = (--(i));
  while (M[((i)++)] = RAM[((j)++)]) {

  }
  return (x);
}

char *(getline_) (const char *(prompt)) {
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

void printchar_ (int b) {
  fputc(b, stdout);
}

int getchar_ () {
  static char *(line) = NULL;
  static char *(ptr) = NULL;
  if ((((!(line))) || ((!(*ptr))))) {
    line = getline_("");
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

int gettoken_ () {
  int c;
  int i = 0;
  do {
    if (((c = getchar_()) > (' '))) {
      RAM[((i)++)] = c;
    }
  } while (((((((c) <= (' '))) || (((c) > (')'))))) && (((dx) > (')')))));
  RAM[i] = 0;
  return (c);
}

int getlist_ () {
  int c = gettoken_();
  if (((c) == (')'))) {
    return (0);
  }
  return (addlist_(getobject_(c)));
}

int getobject_ (int c) {
  if (((c) == ('('))) {
    return (getlist_());
  }
  return (intern_());
}

int addlist_ (int x) {
  return (cons_(x, getlist_()));
}

int read_ () {
  return (getobject_(gettoken_()));
}

int printatom_ (int x) {
  int c;
  for (; ; ) {
    if ((!(c = M[((x)++)]))) {
      break;
    }
    printchar_(c);
  }
}

int printlist_ (int x) {
  printchar_('(');
  printobject_(car_(x));
  while (x = cdr_(x)) {
    if (((x) < (0))) {
      printchar_(' ');
      printobject_(car_(x));
    } else {
      printchar_(L'∙');
      printobject_(x);
      break;
    }
  }
  printchar_(')');
}

int printobject_ (int x) {
  if (((x) < (0))) {
    printlist_(x);
  } else {
    printatom_(x);
  }
}

int print_ (int e) {
  printobject_(e);
}

int printnewline_ () {
  printchar_('\n');
}

int car_ (int x) {
  return (M[x]);
}

int cdr_ (int x) {
  return (M[((x) + (1))]);
}

int cons_ (int car, int cdr) {
  M[(--(cx))] = cdr;
  M[(--(cx))] = car;
  return (cx);
}

int gc_ (int x, int m, int k) {
  if (((x) < (m))) {
    return (((cons_(gc_(car_(x), m, k), gc_(cdr_(x), m, k))) + (k)));
  }
  return (x);
}

int evlis_ (int m, int a) {
  if ((!(m))) {
    return (0);
  }
  int x = eval_(car_(m), a);
  return (cons_(x, evlis_(cdr_(m), a)));
}

int pairlis_ (int x, int y, int a) {
  if ((!(x))) {
    return (a);
  }
  return (cons_(cons_(car_(x), car_(y)), pairlis_(cdr_(x), cdr_(y), a)));
}

int assoc_ (int x, int y) {
  if ((!(y))) {
    return (0);
  } else if ((x) == (car_(car_(y)))) {
    return (cdr_(car_(y)));
  }
  return (assoc_(x, cdr_(y)));
}

int evcon_ (int c, int a) {
  if (eval_(car_(car_(c)), a)) {
    return (eval_(car_(cdr_(car_(c))), a));
  } else {
    return (evcon_(cdr_(c), a));
  }
}

int apply_ (int f, int x, int a) {
  if (((f) < (0))) {
    return (eval_(car_(cdr_(cdr_(f))), pairlis_(car_(cdr_(f)), x, a)));
  } else if ((f) > (kEq)) {
    return (apply_(eval_(f, a), x, a));
  } else if ((f) == (kEq)) {
    return (((((car_(x)) == (car_(cdr_(x))))) ? (kT) : (0)));
  } else if ((f) == (kCons)) {
    return (cons_(car_(x), car_(cdr_(x))));
  } else if ((f) == (kAtom)) {
    return (((((car_(x)) < (0))) ? (0) : (kT)));
  } else if ((f) == (kCar)) {
    return (car_(car_(x)));
  } else if ((f) == (kCdr)) {
    return (cdr_(car_(x)));
  } else if ((f) == (kRead)) {
    return (read_());
  } else if ((f) == (kPrint)) {
    if (x) {
      print_(car_(x));
    } else {
      printnewline_();
    }
    return (0);
  }
}

int eval_ (int e, int a) {
  int A;
  int B;
  int C;
  if (((e) >= (0))) {
    return (assoc_(e, a));
  }
  if (((car_(e)) == (kQuote))) {
    return (car_(cdr_(e)));
  }
  A = cx;
  if (((car_(e)) == (kCond))) {
    e = evcon_(cdr_(e), a);
  } else {
    e = apply_(car_(e), evlis_(cdr_(e), a), a);
  }
  B = cx;
  e = gc_(e, A, ((A) - (B)));
  C = cx;
  while (((C) < (B))) {
    M[(--(A))] = M[(--(B))];
  }
  cx = A;
  return (e);
}

int main () {
  int i;
  setlocale(LC_ALL, "");
  for (i = 0; ((i) < (sizeof(S))); (++(i))) {
    M[i] = S[i];
  }
  for (; ; ) {
    cx = 0;
    print_(eval_(read_(), 0));
    printnewline_();
  }
}


