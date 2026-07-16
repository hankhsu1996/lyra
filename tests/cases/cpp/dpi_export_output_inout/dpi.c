#include <stdint.h>

/* A scalar output / inout crosses by pointer to its by-value carrier; a packed
   bit vector crosses by pointer as a canonical svBitVecVal chunk buffer (LRM
   35.5.6, Annex H.10.1). The emitted program provides each exported function as
   an extern "C" symbol; this imported driver calls them back. */
typedef uint32_t svBitVecVal;

extern void get_pair(int* lo, int* hi);
extern void accumulate(int* acc);
extern void fill_wide(svBitVecVal* w);
extern int sum_wide(const svBitVecVal* w);

int drive(void) {
  int lo = 0;
  int hi = 0;
  get_pair(&lo, &hi); /* lo=3, hi=7 */

  int acc = 5;
  accumulate(&acc); /* acc=105 */

  svBitVecVal w[4] = {0, 0, 0, 0};
  fill_wide(w); /* w = {1, 2, 3, 4} */
  int chunk_sum = (int)(w[0] + w[1] + w[2] + w[3]); /* 10 */

  int s = sum_wide(w); /* w[31:0] + w[127:96] = 1 + 4 = 5 */

  return lo + hi + acc + chunk_sum + s; /* 3+7+105+10+5 = 130 */
}
