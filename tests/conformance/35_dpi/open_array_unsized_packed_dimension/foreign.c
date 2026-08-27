/* Dimension 0 of an open-array handle refers to the packed part, which is
   always one-dimensional; dimensions above 0 refer to the unpacked part, and
   this formal has none (LRM Annex H.12.2). */
#include "dpi.h"

int32_t packed_query(const svOpenArrayHandle h, int32_t which) {
  switch (which) {
    case 0:
      return svDimensions(h);
    case 1:
      return svLeft(h, 0);
    case 2:
      return svRight(h, 0);
    case 3:
      return svLow(h, 0);
    case 4:
      return svHigh(h, 0);
    case 5:
      return svSize(h, 0);
    case 6:
      return svIncrement(h, 0);
    default:
      return -1000;
  }
}
