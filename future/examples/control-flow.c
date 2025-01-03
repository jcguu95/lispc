#include <stdio.h>

int main () {
  int x;
  int y;
  printf("Enter an integer for x:\n");
  scanf("%d", &x);
  printf("Enter an integer for y:\n");
  scanf("%d", &y);
  printf("You entered x = %d and y = %d\n", x, y);
  {
    if (((x) < (0))) {
      goto negative;
    } else {
      printf("x is positive.\n");
    }
    if (((y) < (0))) {
      printf("y is negative.\n");
      {
        negative:
        if (((x) < (0))) {
          printf("x is negative.\n");
        }
      }
      return (0);
    }
  }
}
