/* The disable protocol from the foreign side (LRM 35.9). Each call to the
   exported task is checked: a non-zero return means a disable is active on
   this execution thread, and from that point the protocol allows no further
   call to an exported subroutine and requires this imported task to return 1.

   What each call answered is kept in file scope so the design can read it back
   afterwards through plain imported functions, which is the only channel left
   once the protocol has closed the exported ones. */
#include <svdpi.h>

#include "dpi.h"

static int32_t calls = 0;
static int32_t last = 0;
static int32_t queried = 0;

int32_t advance(int32_t rounds) {
  int32_t i;
  for (i = 0; i < rounds; i++) {
    calls++;
    last = step(i + 2);
    if (last != 0) {
      queried = svIsDisabledState();
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

int32_t queried_state(void) {
  return queried;
}
