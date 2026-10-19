#include <stddef.h>

#include "dpi.h"

static int32_t g_calls = 0;

int32_t g_via_c(void) {
  ++g_calls;
  return read_g();
}

int32_t g_via_c_calls(void) {
  return g_calls;
}

int32_t level_via_c(void) {
  return read_level();
}

int32_t deep_via_c(void) {
  return read_deep();
}

/* The export belongs to another instance, so the call chain is moved there
   first (LRM 35.5.3). */
int32_t held_via_c(const char* path) {
  svScope target = svGetScopeFromName(path);
  if (target == NULL) {
    return -1;
  }
  svSetScope(target);
  return read_held();
}
