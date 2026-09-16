/* Two imported tasks, one reached from inside the exported task the other
   called, and an exported function reached from that inner one. Neither frame
   here holds anything across the suspensions the exported task executes around
   the inner call, which is what LRM 35.5.1.5 asks of C code that can be active
   in more than one execution thread. */
#include "dpi.h"

int32_t outer_call(int32_t amount) {
  middle(amount);
  return 0;
}

int32_t inner_call(int32_t value) {
  note(value + 1);
  return 0;
}
