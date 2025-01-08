#include <stdio.h>

int main () {
  int i;
  int j;
  printf("Enter an integer for i: ");
  scanf("%d", &i);
  switch (i) {
    case 1:
      printf("i = 1\n");
      break;
    case 2:
      printf("i = 2\n");
      break;
    case 3:
      printf("i = 3\n");
      printf("Enter an integer for j: ");
      scanf("%d", &j);
      switch (j) {
        case 1:
          printf("j = 1\n");
          break;
        default:
          printf("Wrong guess. Aborting..\n");
          break;
      }
      break;
    default:
      printf("Wrong guess. Aborting..\n");
      break;
  }
  return (0);
}
