/* An imported function whose call to an exported function left this execution
   in the disabled state (LRM 35.9). There is no return value to read it from,
   so it is asked for and acknowledged before returning, which is what the
   protocol requires of an imported function. */
#include <svdpi.h>

#include "dpi.h"

static int32_t queried = 0;

int32_t ask(int32_t seed) {
  int32_t got = answer(seed);
  queried = svIsDisabledState();
  if (queried != 0) {
    svAckDisabledState();
  }
  return got + 1;
}

int32_t queried_state(void) {
  return queried;
}
