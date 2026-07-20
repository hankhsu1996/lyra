#include <string.h>

typedef void* svScope;
extern svScope svGetScope(void);
extern const char* svGetNameFromScope(svScope scope);

/* Exported SV tasks that consume time, driven across the boundary so the calling
   import task suspends. The long one outlives the short one, so the two imports
   are suspended concurrently. */
extern int nap_long(void);
extern int nap_short(void);

/* Reads the observing scope, suspends inside the exported task, then confirms
   the scope observed after the suspension is the same one -- its own
   declaration scope -- not the other concurrent import's. */
static void observe(int (*nap)(void), int* ok) {
  char before[64];
  const char* first = svGetNameFromScope(svGetScope());
  if (first == 0) {
    *ok = 0;
    return;
  }
  strncpy(before, first, sizeof(before) - 1);
  before[sizeof(before) - 1] = 0;

  nap();

  const char* after = svGetNameFromScope(svGetScope());
  *ok = (after != 0 && strcmp(before, after) == 0) ? 1 : 0;
}

void observe_long(int* ok) {
  observe(nap_long, ok);
}

void observe_short(int* ok) {
  observe(nap_short, ok);
}
