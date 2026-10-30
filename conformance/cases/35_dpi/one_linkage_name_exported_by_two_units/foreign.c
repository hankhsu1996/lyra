/* One entry point for a name two scopes each export. Which subroutine answers
   is decided by the call chain's scope, never by anything written here. */
#include <svdpi.h>

/* The exported subroutines called from here, declared as the design
   exports them (LRM 35.5.6, Annex H). */
int32_t shared_scale(int32_t);

int32_t call_shared(int32_t x) {
  return shared_scale(x);
}
