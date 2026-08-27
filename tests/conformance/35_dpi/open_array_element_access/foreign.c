/* Each entry walks the indices the actual declared and touches its elements
   through the accessor its element type calls for: a canonical copy for a
   packed element, a direct read or write for a scalar one (LRM Annex H.12.3,
   H.12.5, H.12.6). */
#include "dpi.h"

/* What is written depends on both the seed and the index, so a written
   element differs from every other and from what the actual held. */
void fill(int32_t seed, const svOpenArrayHandle h) {
  int i;
  for (i = svLow(h, 1); i <= svHigh(h, 1); i++) {
    svBitVecVal element = (svBitVecVal)(seed + i);
    svPutBitArrElem1VecVal(h, &element, i);
  }
}

/* An element whose unknown plane is clear is advanced; one carrying an x or a
   z is left as it stands. Both planes therefore have to arrive intact, and the
   advanced ones have to travel back. */
void bump(const svOpenArrayHandle h) {
  int i;
  for (i = svLow(h, 1); i <= svHigh(h, 1); i++) {
    svLogicVecVal element;
    svGetLogicArrElem1VecVal(&element, h, i);
    if ((element.bval & 0xFFFFu) == 0) {
      element.aval = (element.aval + 10u) & 0xFFFFu;
      svPutLogicArrElem1VecVal(h, &element, i);
    }
  }
}

int32_t element_at(const svOpenArrayHandle h, int32_t index) {
  svBitVecVal element;
  svGetBitArrElem1VecVal(&element, h, index);
  return (int32_t)element;
}

/* Two unsized unpacked dimensions, each coordinate carrying its own weight, so
   a row read as a column changes the total. */
int32_t trace(const svOpenArrayHandle h) {
  int32_t total = 0;
  int i;
  int j;
  if (svDimensions(h) != 2) {
    return -1;
  }
  for (i = svLow(h, 1); i <= svHigh(h, 1); i++) {
    for (j = svLow(h, 2); j <= svHigh(h, 2); j++) {
      svBitVecVal element;
      svGetBitArrElem2VecVal(&element, h, i, j);
      total += ((i * 10) + j) * (int32_t)element;
    }
  }
  return total;
}

/* A one-bit element needs no buffer; each is read on its own and weighted by
   its position, so the order the elements arrived in is part of the answer. */
int32_t scalar_digest(const svOpenArrayHandle h) {
  int32_t total = 0;
  int32_t weight = 1;
  int i;
  for (i = svLow(h, 1); i <= svHigh(h, 1); i++) {
    total += weight * (int32_t)svGetBitArrElem1(h, i);
    weight *= 2;
  }
  return total;
}

/* A four-state scalar element is written directly, and the two values written
   differ from each other and from what the actual held. */
void set_marks(int32_t mask, const svOpenArrayHandle h) {
  int i;
  int32_t position = 0;
  for (i = svLow(h, 1); i <= svHigh(h, 1); i++) {
    svLogic value = ((mask >> position) & 1) != 0 ? sv_1 : sv_z;
    svPutLogicArrElem1(h, value, i);
    position++;
  }
}
