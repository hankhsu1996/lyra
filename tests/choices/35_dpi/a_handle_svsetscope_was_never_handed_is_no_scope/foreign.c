/* Installs as the current scope the address of a static C object, which the C
   layer never gave out (LRM Annex H.9.3). */
#include <stddef.h>
#include <svdpi.h>

/* The exported subroutine called from here, declared as the design exports
   it (LRM 35.5.6, Annex H). */
void reached(void);

static const int64_t not_a_scope[8];
static const int user_key = 0;

/* One bit per check, so a failure names it: svGetScope reports the handle
   installed, and reporting it does not make it a scope, so storing user data
   under it and asking its name both still find none. The original scope is
   put back before returning. */
int32_t set_scope_never_handed_out(void) {
  svScope invalid = (svScope)not_a_scope;
  svScope original = svSetScope(invalid);
  svScope reported = svGetScope();
  int32_t result = 0;
  if (reported == invalid) {
    result |= 1;
  }
  if (svPutUserData(reported, (void*)&user_key, (void*)1) == -1) {
    result |= 2;
  }
  if (svGetNameFromScope(reported) == NULL) {
    result |= 4;
  }
  svSetScope(original);
  return result;
}

/* Should the export return to this frame, the disable protocol is followed for
   whatever state it finds, so the report the case asks for is the only one. */
void call_under_a_scope_never_handed_out(void) {
  svSetScope((svScope)not_a_scope);
  reached();
  if (svIsDisabledState() != 0) {
    svAckDisabledState();
  }
}
