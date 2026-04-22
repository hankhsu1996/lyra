#include <stddef.h>

#include "svdpi.h"

extern void set_state(int val);
extern int get_state(void);

int call_task_export(void) {
  svScope scope = svGetScopeFromName("Test.inner");
  if (scope == NULL) return 0;
  svSetScope(scope);
  set_state(42);
  int val = get_state();
  return (val == 42) ? 1 : 0;
}
