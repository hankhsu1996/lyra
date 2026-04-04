#include <stddef.h>

#include "svdpi.h"

/* Module export: integer (4-state 32-bit) return.
   Indirect return convention: void wrapper(svLogicVecVal* result). */
extern void get_mod_integer(svLogicVecVal* result);

int call_get_mod_integer(void) {
  svScope scope = svGetScopeFromName("Test.inner_inst");
  if (scope == NULL) return 0;
  svSetScope(scope);
  svLogicVecVal result;
  get_mod_integer(&result);
  return (result.aval == 99 && result.bval == 0) ? 1 : 0;
}
