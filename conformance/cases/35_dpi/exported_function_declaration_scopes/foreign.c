/* Two imports, each declared in the same scope as the export it calls, so
   neither has to choose a scope before calling back (LRM 35.5.3). The
   exported name is a plain global symbol wherever its scope was
   (LRM 35.4, 35.7); nothing here records which scope that was. */
#include <svdpi.h>

/* The exported subroutines called from here, declared as the design
   exports them (LRM 35.5.6, Annex H). */
int32_t module_double(int32_t);
int32_t block_quintuple(int32_t);

int32_t call_module(int32_t x) {
  return (module_double(x) * 10) + x;
}

int32_t call_block(int32_t x) {
  return (block_quintuple(x) * 10) + x;
}
