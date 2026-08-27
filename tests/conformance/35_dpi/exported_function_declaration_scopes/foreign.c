/* Four imports, each declared in the same scope as the export it calls, so
   none of them has to choose a scope before calling back (LRM 35.5.3). The
   exported name is a plain global symbol wherever its scope was
   (LRM 35.4, 35.7); nothing here records which scope that was. */
#include "dpi.h"

int32_t call_module(int32_t x) {
  return (module_double(x) * 10) + x;
}

int32_t call_pkg(int32_t x) {
  return (pkg_triple(x) * 10) + x;
}

int32_t call_unit(int32_t x) {
  return (unit_quadruple(x) * 10) + x;
}

int32_t call_block(int32_t x) {
  return (block_quintuple(x) * 10) + x;
}
