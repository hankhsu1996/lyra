/* The same two entry points serve every instance and every replica; which one
   answers is decided by the call chain's scope, never by anything written
   here. */
#include <stddef.h>

#include "dpi.h"

/* This import was declared in the enclosing module, so its chain starts there
   and the export it wants belongs to another scope. Naming that scope and
   installing it is what makes the call reach the right instance
   (LRM 35.5.3). */
int32_t read_at(const char* path) {
  svScope target = svGetScopeFromName(path);
  if (target == NULL) {
    return -1;
  }
  svSetScope(target);
  return read_id();
}

/* This one was declared in the same generate block as the export, so the chain
   already holds the scope the call needs (LRM 35.5.3). */
int32_t call_read(void) {
  return read_tag();
}
