#include <svdpi.h>

int ctx_fn(svScope scope, int x) {
  (void)scope;
  return x;
}

int pure_fn(int x) {
  return x;
}
