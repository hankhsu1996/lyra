/* The generated ABI header declares every open-array prototype, so each
   handle argument is spelled the way LRM Annex H.8.6 fixes it. Every entry
   here reads the declared indices the SystemVerilog source uses; none assumes
   a normalized range. */
#include "dpi.h"

/* An index-weighted sum, so an element reached under the wrong index changes
   the result rather than cancelling out. */
int32_t weigh(const svOpenArrayHandle h) {
  int total = 0;
  for (int i = svLow(h, 1); i <= svHigh(h, 1); i++) {
    svBitVecVal w;
    svGetBitArrElem1VecVal(&w, h, i);
    total += i * (int)(signed char)(w & 0xff);
  }
  return (int32_t)total;
}

/* The dimension queries over a descending declared range. A `byte` element's
   canonical form is not how an individual `byte` crosses (Annex H.7.7 versus
   Table H.1), so the addressing entries answer with a null. */
int32_t describe(const svOpenArrayHandle h) {
  if (svDimensions(h) != 1) return -1;
  if (svIncrement(h, 1) != 1) return -2;
  if (svGetArrayPtr(h) != 0) return -3;
  if (svSizeOfArray(h) != 0) return -4;
  return (int32_t)(svLeft(h, 1) * 10000 + svRight(h, 1) * 1000 +
                   svLow(h, 1) * 100 + svHigh(h, 1) * 10 + svSize(h, 1));
}

/* An `output` open array: the space is the actual's, and what the C code
   leaves in it is what the actual holds after the call. */
void fill(const svOpenArrayHandle h) {
  svBitVecVal w = 7;
  for (int i = svLow(h, 1); i <= svHigh(h, 1); i++) {
    svPutBitArrElem1VecVal(h, &w, i);
    w++;
  }
}

/* A four-state packed element crosses in both planes, so an element that is
   partly unknown stays unknown across the boundary. */
void bump(const svOpenArrayHandle h) {
  for (int i = svLow(h, 1); i <= svHigh(h, 1); i++) {
    svLogicVecVal v;
    svGetLogicArrElem1VecVal(&v, h, i);
    if (v.bval == 0) {
      v.aval += 10;
      svPutLogicArrElem1VecVal(h, &v, i);
    }
  }
}

/* Two unsized unpacked dimensions, weighted per coordinate so a row-major
   mix-up changes the result. */
int32_t trace(const svOpenArrayHandle h) {
  int total = 0;
  if (svDimensions(h) != 2) return -1;
  for (int i = svLow(h, 1); i <= svHigh(h, 1); i++) {
    for (int j = svLow(h, 2); j <= svHigh(h, 2); j++) {
      svBitVecVal w;
      svGetBitArrElem2VecVal(&w, h, i, j);
      total += ((i * 10) + j) * (int)w;
    }
  }
  return (int32_t)total;
}

/* An unsized packed dimension: the array has no unpacked dimension, and
   dimension 0 reports the linearized, normalized range of the actual's packed
   dimensions (LRM Annex H.7.6). */
int32_t width_of(const svOpenArrayHandle h) {
  if (svDimensions(h) != 0) return -1;
  if (svIncrement(h, 0) != 1) return -2;
  return (int32_t)(svLeft(h, 0) * 100 + svRight(h, 0) * 10 + svSize(h, 0));
}

/* A packed-vector element does cross as its canonical form individually, so
   the array and its elements have addresses (Annex H.12.4). */
int32_t addressable(const svOpenArrayHandle h) {
  const svBitVecVal* p;
  if (svGetArrayPtr(h) == 0) return -1;
  if (svSizeOfArray(h) != 8) return -2;
  p = (const svBitVecVal*)svGetArrElemPtr1(h, 1);
  if (p == 0) return -3;
  return (int32_t)(*p == 0x22222222u);
}
