/* One definition per imported name, whichever SystemVerilog scope declared it:
   the foreign name space is global and holds no trace of that scope
   (LRM 35.4). */
#include "dpi.h"

int32_t scale(int32_t a, int32_t b) {
  return (a * 10) + b;
}

/* Import and export were declared in the same package, so the export is
   reachable from here without setting a scope first (LRM 35.5.3). Its result
   and the argument both reach the answer, so neither can be missing from it. */
int32_t probe(int32_t a) {
  return (pkg_double(a) * 100) + a;
}

void split(int32_t a, int32_t* lo, int32_t* hi) {
  *lo = a % 100;
  *hi = a / 100;
}

int32_t negate(int32_t a) {
  return -a;
}
