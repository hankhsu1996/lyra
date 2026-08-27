/* The loop below is suspended and resumed once per iteration: each call to the
   exported task blocks on a delay, so this frame stays on the stack while
   simulation time advances (LRM 35.5.1.5). Standard reentrancy care applies to
   C code reached this way, so nothing here is kept outside the frame.

   Every suspension is one step longer than the one before it, so how many
   there were and how long the first one was reach the answer separately
   rather than only as their product. */
#include "dpi.h"

int32_t advance(int32_t rounds, int32_t amount) {
  int32_t i;
  for (i = 0; i < rounds; i++) {
    step(amount + i);
  }
  return 0;
}
