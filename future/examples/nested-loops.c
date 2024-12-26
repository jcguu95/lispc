#include <stdio.h>

int main () {
  for (int i = 0; ((i) < (3)); ((i)++)) {
    for (int j = 0; ((j) < (2)); ((j)++)) {
      for (int k = 0; ((k) < (2)); ((k)++)) {
        printf("%d ", ((i) + (j) + (k)));
        printf("%d\n", i);
      };
    };
  };
  return (0);
}
