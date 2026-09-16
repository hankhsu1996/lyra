/* One entry point for a name two scopes each export. Which subroutine answers
   is decided by the call chain's scope, never by anything written here. */
#include "dpi.h"

int32_t call_shared(int32_t x) {
  return shared_scale(x);
}
