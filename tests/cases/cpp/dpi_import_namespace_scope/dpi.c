/* The generated ABI header declares every name this design takes part in,
   including the package-scoped export `probe` calls back, so this source
   states no prototype of its own. */
#include "dpi.h"

int32_t scale(int32_t a) { return a * 2; }

/* Declared in a package, which is never instantiated, so this context import
   observes no scope (LRM 35.5.3) -- and a receiver-less export is still
   directly reachable from it, because it needs no instance to dispatch to. */
int32_t probe(int32_t a) {
  return (svGetScope() == 0 ? 100 : 200) + pkg_double(a);
}

void split(int32_t a, int32_t* lo, int32_t* hi) {
  *lo = a % 100;
  *hi = a / 100;
}

int32_t negate(int32_t a) { return -a; }
