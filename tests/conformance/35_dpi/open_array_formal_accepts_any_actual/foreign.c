/* An open array reaches this side as a handle rather than as an address, and
   everything about the actual is asked of that handle (LRM Annex H.8.6,
   H.12.2). Both entries below run over whatever indices the actual declared,
   so neither assumes a range of its own. */
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
int32_t query(const svOpenArrayHandle h, int32_t which) {
  switch (which) {
    case 0:
      return svDimensions(h);
    case 1:
      return svLeft(h, 1);
    case 2:
      return svRight(h, 1);
    case 3:
      return svLow(h, 1);
    case 4:
      return svHigh(h, 1);
    case 5:
      return svSize(h, 1);
    case 6:
      return svIncrement(h, 1);
    default:
      return -1000;
  }
}
