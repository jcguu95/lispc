#include <stdio.h>

#define EQUALP(a,b) \
((a) == (b))

#undef EQUALP

#define SWAP(a,b) \
do { \
  typeof(a) temp = a; \
  a = b; \
  b = temp; \
} while (0)

#undef SWAP

#define EQUALP(a,b) \
((a) == (b))

#define SWAP(a,b) \
do { \
  typeof(a) temp = a; \
  a = b; \
  b = temp; \
} while (0)

int main () {
  int x = 5;
  int y = 10;
  SWAP(x, y);
  printf("x = %d, y = %d\n", x, y);
  return (0);
}

#undef EQUALP

#undef SWAP
