/* Reports the scope the calling context import was declared in, by name (LRM
   35.5.3, Annex H.9.1). A declaration in a scope that is never instantiated has
   none, which the C layer reports as a null handle; the name of that is spelled
   here so the design compares two strings either way. */
#include <stddef.h>

#include "dpi.h"

const char* where(void) {
  svScope scope = svGetScope();
  return scope == NULL ? "<no scope>" : svGetNameFromScope(scope);
}
