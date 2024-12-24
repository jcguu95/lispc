#include <stdio.h>

int main () {
  int i = 20;
  int j = 30;
  if ((i) == (10)) {
    printf("Hello!\n");
    printf("i is 10\n");
  } else if ((((i) == (15))) || (((i) == (20)))) {
    printf("Hello!\n");
    printf("i is 15 or 20\n");
  } else if ((((i) > (0))) && (((i) < (30)))) {
    printf("Hello!\n");
    printf("i is between 0 and 30\n");
    if ((j) == (30)) {
      printf("j is equal to 30.");
    } else {
      printf("j is not equal to 30.");
    };
  } else {
    printf("Hello!\n");
    printf("i is not present\n");
  };
  return (0);
}
