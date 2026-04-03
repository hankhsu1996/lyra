#include <stddef.h>

#include "svdpi.h"

extern int get_x(void);

int call_same_body_instances(void) {
  svScope sa = svGetScopeFromName("Test.a");
  svScope sb = svGetScopeFromName("Test.b");
  if (sa == NULL || sb == NULL) return 0;
  svSetScope(sa);
  int ax = get_x();
  svSetScope(sb);
  int bx = get_x();
  return (ax == 101 && bx == 202) ? 1 : 0;
}
