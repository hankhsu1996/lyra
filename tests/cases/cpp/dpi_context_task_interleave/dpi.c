#include <string.h>

/* The exported SV task is driven across the boundary so the calling import task
   suspends. Both instances run the same import, so the two calls overlap: the
   longer nap outlives the shorter one. The generated ABI header declares the
   export and, via the standard header, the svdpi scope surface. */
#include "dpi.h"

/* Reads the observing scope, suspends inside the exported task, then confirms
   the scope observed after the suspension is the same one -- this instance's --
   not the other concurrent import's. */
int observe(int* ok) {
  char before[64];
  const char* first = svGetNameFromScope(svGetScope());
  if (first == 0) {
    *ok = 0;
    return 0;
  }
  strncpy(before, first, sizeof(before) - 1);
  before[sizeof(before) - 1] = 0;

  nap();

  const char* after = svGetNameFromScope(svGetScope());
  *ok = (after != 0 && strcmp(before, after) == 0) ? 1 : 0;
  return 0;
}
