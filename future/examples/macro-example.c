#include <stdio.h>

int foo_INT (int x, int y) {
  return (((2) * (((x) + (y)))));
};
float foo_FLOAT (float x, float y) {
  return (((2) * (((x) + (y)))));
};
double foo_DOUBLE (double x, double y) {
  return (((2) * (((x) + (y)))));
};

int main () {
  int int_x = 5;
  int int_y = 10;
  float float_x = 2.5f;
  float float_y = 3.5f;
  double double_x = 1.234;
  double double_y = 4.567;
  int int_result = foo_INT(int_x, int_y);
  float float_result = foo_FLOAT(float_x, float_y);
  double double_result = foo_DOUBLE(double_x, double_y);
  printf("foo_INT(%d, %d) = %d\n", int_x, int_y, int_result);
  printf("foo_FLOAT(%.2f, %.2f) = %.2f\n", float_x, float_y, float_result);
  printf("foo_DOUBLE(%.3f, %.3f) = %.3f\n", double_x, double_y, double_result);
}
