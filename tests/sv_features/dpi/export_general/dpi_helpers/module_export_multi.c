#include <stddef.h>

#include "svdpi.h"

extern int get_id(void);
extern int get_id_plus_one(void);

int call_both_exports(void) {
  svScope sa = svGetScopeFromName("Test.a");
  svScope sb = svGetScopeFromName("Test.b");
  if (sa == NULL || sb == NULL) return 0;
  svSetScope(sa);
  int a0 = get_id();
  int a1 = get_id_plus_one();
  svSetScope(sb);
  int b0 = get_id();
  int b1 = get_id_plus_one();
  return (a0 == 11 && a1 == 12 && b0 == 22 && b1 == 23) ? 1 : 0;
}
