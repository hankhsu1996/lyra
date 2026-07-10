#include <string.h>

int add_one(int x) {
  return x + 1;
}

int mul(int a, int b) {
  return a * b;
}

double scale(double r, int k) {
  return r * k;
}

double negate(double r) {
  return -r;
}

int slen(const char* s) {
  return (int)strlen(s);
}
