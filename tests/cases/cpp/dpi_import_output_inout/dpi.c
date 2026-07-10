#include <stdio.h>

void get_int(int* x) {
  *x = 42;
}

void get_real(double* r) {
  *r = 2.5;
}

void get_str(const char** s) {
  *s = "hello";
}

void bump_int(int* x) {
  *x = *x + 5;
}

void scale_real(double* r) {
  *r = *r * 2.0;
}

void suffix_str(const char** s) {
  static char buf[64];
  snprintf(buf, sizeof(buf), "%s!", *s);
  *s = buf;
}

int divmod(int a, int b, int* rem) {
  *rem = a % b;
  return a / b;
}
