/* One import serving both instances. The scope it reads is the chain's, not
   anything this file holds, and the exported task it calls belongs to the same
   scope, so the value reported names the instance the chain came from
   (LRM 35.5.3). The design supplies the name its own scope has, so what the
   chain observes is held against a scope named from outside rather than only
   against itself. */
#include "dpi.h"

int32_t observe(const char* path, int32_t amount, int32_t* verdict) {
  svScope before = svGetScope();
  int32_t reported = -1;
  svScope after;

  /* The chain suspends inside this call while the other instance's chain runs
     and pushes its own scope. */
  nap_and_report(amount, &reported);

  after = svGetScope();
  if (before != after || after != svGetScopeFromName(path)) {
    *verdict = -1;
    return 0;
  }
  *verdict = reported;
  return 0;
}
