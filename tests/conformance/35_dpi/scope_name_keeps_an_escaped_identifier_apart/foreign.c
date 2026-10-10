/* The name of the scope a context import was declared in, and whether a name
   finds that scope again (LRM Annex H.9.3). Every answer is read back out to
   the design rather than judged here. */
#include <stddef.h>

#include "dpi.h"

const char* scope_name(void) {
  return svGetNameFromScope(svGetScope());
}

/* One bit per check, so a failure names it: `name` finds the scope the import
   was declared in, and `other` finds a scope that is not it. */
int32_t name_finds_this_scope(const char* name, const char* other) {
  svScope here = svGetScope();
  svScope elsewhere = svGetScopeFromName(other);
  int32_t result = 0;
  if (svGetScopeFromName(name) == here) {
    result |= 1;
  }
  if (elsewhere != NULL && elsewhere != here) {
    result |= 2;
  }
  return result;
}
