/* Two imports, one declared in a package and one in the compilation unit,
   each calling the export declared beside it without choosing a scope. */
#include <svdpi.h>

/* The exported subroutines called from here, declared as the design
   exports them (LRM 35.5.6, Annex H). */
int32_t pkg_triple(int32_t);
int32_t unit_quadruple(int32_t);

int32_t call_pkg(int32_t x) {
  return (pkg_triple(x) * 10) + x;
}

int32_t call_unit(int32_t x) {
  return (unit_quadruple(x) * 10) + x;
}
