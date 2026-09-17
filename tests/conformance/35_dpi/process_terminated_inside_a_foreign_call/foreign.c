/* The same protocol LRM 35.9 states for a disabled block, seen from the
   foreign side when what ended is the process itself: a non-zero return from
   an exported task means this execution thread must stop, so no further
   exported subroutine is called and the imported task returns 1. */
#include "dpi.h"

static int32_t calls = 0;
static int32_t last = 0;

int32_t advance(int32_t rounds) {
  int32_t i;
  for (i = 0; i < rounds; i++) {
    calls++;
    last = step(i + 2);
    if (last != 0) {
      return 1;
    }
  }
  return 0;
}

int32_t calls_made(void) {
  return calls;
}

int32_t last_return(void) {
  return last;
}
