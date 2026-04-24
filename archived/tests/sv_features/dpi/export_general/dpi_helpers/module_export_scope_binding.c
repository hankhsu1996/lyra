#include <stddef.h>

#include "svdpi.h"

extern int get_id(void);

int call_module_exports(void) {
  svScope sa = svGetScopeFromName("Test.a");
  svScope sb = svGetScopeFromName("Test.b");
  if (sa == NULL || sb == NULL) return 0;
  svSetScope(sa);
  int ra = get_id();
  svSetScope(sb);
  int rb = get_id();
  return (ra == 11 && rb == 22) ? 1 : 0;
}
