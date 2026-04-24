#include <stddef.h>

#include "svdpi.h"

/* Package export: integer return with X bits.
   Checks that bval (X/Z encoding) roundtrips correctly. */
extern void get_integer_with_x(svLogicVecVal* result);

int call_get_integer_with_x(void) {
  svLogicVecVal result;
  get_integer_with_x(&result);
  /* SV function returns integer'(32'hF0), with low 4 bits forced X.
     aval should be 0xFF (aval = val ^ unk = 0xF0 ^ 0x0F = 0xFF).
     bval should be 0x0F. */
  return (result.aval == 0xFF && result.bval == 0x0F) ? 1 : 0;
}
