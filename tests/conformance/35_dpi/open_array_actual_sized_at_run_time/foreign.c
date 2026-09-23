/* An open array reaches this side as a handle, and everything about the actual
   is asked of that handle (LRM Annex H.8.6, H.12.2). Neither entry assumes a
   range of its own, so the same code serves an actual whose extent the
   SystemVerilog side only fixed while it was running. */
#include "dpi.h"

/* A byte element is a packed type, so the array holds it in canonical form and
   it is read one chunk at a time (LRM Annex H.7.3, H.12.5). */
int32_t weigh(const svOpenArrayHandle h) {
  int32_t total = 0;
  int i;
  for (i = svLow(h, 1); i <= svHigh(h, 1); i++) {
    svBitVecVal element;
    svGetBitArrElem1VecVal(&element, h, i);
    total += i * (int32_t)(int8_t)(element & 0xFFu);
  }
  return total;
}

/* One query per selector, so a wrong answer names the query that gave it
   rather than being folded into a digest. */
int32_t bounds_of(const svOpenArrayHandle h, int32_t which) {
  switch (which) {
    case 0:
      return svDimensions(h);
    case 1:
      return svLeft(h, 1);
    case 2:
      return svRight(h, 1);
    case 3:
      return svSize(h, 1);
    case 4:
      return svIncrement(h, 1);
    default:
      return -1000;
  }
}
