#include <stdio.h>
#include <stdlib.h>

typedef struct x x;

struct x {
  int value;
  x* next;
};

int main () {
  x* x1 = malloc(sizeof(x));
  x* x2 = malloc(sizeof(x));
  x* x3 = malloc(sizeof(x));
  x1->value = 10;
  x2->value = 20;
  x3->value = 30;
  x1->next = x2;
  x2->next = x3;
  printf("x1->value             = %d\n", x1->value);
  printf("x1->next->value       = %d\n", x1->next->value);
  printf("x1->next->next->value = %d\n", x1->next->next->value);
  return (0);
}
