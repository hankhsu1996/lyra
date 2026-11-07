/* Reports the scope the calling context import was declared in, by name (LRM
   35.5.3, Annex H.9.1), or that it was handed none. */
#include <stddef.h>
#include <svdpi.h>

const char* where(void) {
  svScope scope = svGetScope();
  return scope == NULL ? "<no scope>" : svGetNameFromScope(scope);
}
