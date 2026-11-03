/* One definition per imported name, called from whichever instance's
   declaration a call names (LRM 35.4). */
#include <svdpi.h>

/* The exported subroutine called from here, declared as the design exports
   it (LRM 35.5.6, Annex H). */
int32_t own_mine(void);

int32_t twice(int32_t x) {
  return 2 * x;
}

int32_t twice_of_package(int32_t x) {
  return 2 * x;
}

int32_t add(int32_t a, int32_t b) {
  return a + b;
}

int32_t sum_of(const svOpenArrayHandle a, int32_t n) {
  int32_t total = 0;
  for (int32_t i = 0; i < n; i++) {
    svBitVecVal element = 0;
    svGetBitArrElem1VecVal(&element, a, svLow(a, 1) + i);
    total += (int32_t)element;
  }
  return total;
}

int32_t rows_plus(const svOpenArrayHandle a, int32_t n) {
  return n + svSize(a, 1);
}

void fill(int32_t* x) {
  *x = 77;
}

/* A context import observes its declaring instance, so the export called
   back reads that instance's variable (LRM 35.5.3). Both the argument and the
   export's answer reach the result, so neither can be missing from it. */
int32_t through_export(int32_t x) {
  return (x * 100) + own_mine();
}
