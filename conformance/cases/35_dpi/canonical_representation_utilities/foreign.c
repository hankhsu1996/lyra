/* Each import hands its arguments to one utility of the canonical
   representation (LRM Annex H.11.5) and answers what it answered. A
   part-select written from a source group takes the group by value, so the
   first group of the 32-bit source vector is what it is handed. */
#include <svdpi.h>

svBit get_bit(const svBitVecVal* v, int i) {
  return svGetBitselBit(v, i);
}

svLogic get_logic(const svLogicVecVal* v, int i) {
  return svGetBitselLogic(v, i);
}

void put_bit(svBitVecVal* v, int i, svBit s) {
  svPutBitselBit(v, i, s);
}

void put_logic(svLogicVecVal* v, int i, svLogic s) {
  svPutBitselLogic(v, i, s);
}

void get_part_bit(svBitVecVal* d, const svBitVecVal* v, int i, int w) {
  svGetPartselBit(d, v, i, w);
}

void get_part_logic(svLogicVecVal* d, const svLogicVecVal* v, int i, int w) {
  svGetPartselLogic(d, v, i, w);
}

void put_part_bit(svBitVecVal* d, const svBitVecVal* s, int i, int w) {
  svPutPartselBit(d, s[0], i, w);
}

void put_part_logic(svLogicVecVal* d, const svLogicVecVal* s, int i, int w) {
  svPutPartselLogic(d, s[0], i, w);
}

const char* dpi_version(void) {
  return svDpiVersion();
}
