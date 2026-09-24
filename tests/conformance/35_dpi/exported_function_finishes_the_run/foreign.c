/* An imported function whose call to an exported function ended the
   simulation. The export returns to this frame rather than unwinding through
   it, which is what the flag records. Whatever the disabled-state query
   answers, the protocol is followed for it: acknowledged when it is set. */
#include <svdpi.h>

#include "dpi.h"

static int32_t returned = 0;

int32_t ask(int32_t seed) {
  int32_t got = answer(seed);
  returned = 1;
  if (svIsDisabledState() != 0) {
    svAckDisabledState();
  }
  return got;
}

int32_t returned_normally(void) {
  return returned;
}
