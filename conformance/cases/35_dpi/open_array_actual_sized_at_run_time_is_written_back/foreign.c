/* An open array reaches this side as a handle whose range is the actual's
   (LRM Annex H.7.6, H.12.2), so neither entry assumes a size of its own. */
#include <svdpi.h>

/* Each element of the actual doubled where it lies, read and written one
   canonical chunk at a time (LRM Annex H.7.3, H.12.5). */
void doubled(const svOpenArrayHandle h) {
  for (int i = svLow(h, 1); i <= svHigh(h, 1); i++) {
    svBitVecVal element = 0;
    svGetBitArrElem1VecVal(&element, h, i);
    const svBitVecVal twice = (element * 2U) & 0xFFU;
    svPutBitArrElem1VecVal(h, &twice, i);
  }
}

/* Each element of each row doubled where it lies, the rows' count and each
   row's width both asked of the handle. */
void rows_doubled(const svOpenArrayHandle h) {
  for (int i = svLow(h, 1); i <= svHigh(h, 1); i++) {
    for (int j = svLow(h, 2); j <= svHigh(h, 2); j++) {
      svBitVecVal element = 0;
      svGetBitArrElem2VecVal(&element, h, i, j);
      const svBitVecVal twice = (element * 2U) & 0xFFU;
      svPutBitArrElem2VecVal(h, &twice, i, j);
    }
  }
}

/* Each element of an output actual written with its position counted from 1,
   whatever the actual held before. */
void numbered(const svOpenArrayHandle h) {
  for (int i = svLow(h, 1); i <= svHigh(h, 1); i++) {
    const svBitVecVal number = (svBitVecVal)(i - svLow(h, 1) + 1);
    svPutBitArrElem1VecVal(h, &number, i);
  }
}
