/* The foreign side of a call whose process was terminated from outside. It
   follows LRM 35.9 for whatever the exported task answers: a non-zero return
   means this execution thread must stop, so no further exported subroutine is
   called and the imported task returns 1. The rounds are few enough that the
   loop ends on its own either way, so the count of calls is the same whichever
   answer the tool gives. */
#include "dpi.h"

static int32_t calls = 0;

int32_t advance(int32_t rounds) {
  int32_t i;
  for (i = 0; i < rounds; i++) {
    calls++;
    if (step(i + 2) != 0) {
      return 1;
    }
  }
  return 0;
}

int32_t calls_made(void) {
  return calls;
}
