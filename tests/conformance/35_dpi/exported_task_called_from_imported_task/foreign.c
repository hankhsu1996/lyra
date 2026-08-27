/* An imported task is written here as a function returning int, and so is
   every exported task it calls: no disable is active, so each of those calls
   has to report 0 (LRM 35.8, 35.9). */
#include "dpi.h"

int32_t drive(int32_t seed, int32_t* verdict) {
  int32_t checks = 0;
  int32_t lo = -1;
  int32_t hi = -2;
  int32_t acc = 5;

  if (add_delta(seed) == 0) {
    checks |= 1;
  }
  if (add_delta(seed + 1) == 0) {
    checks |= 2;
  }

  if (scale_pair(seed, &lo, &hi) == 0) {
    checks |= 4;
  }
  if (lo == 8) {
    checks |= 8;
  }
  if (hi == 12) {
    checks |= 16;
  }

  if (accumulate(&acc) == 0) {
    checks |= 32;
  }
  if (acc == 53) {
    checks |= 64;
  }

  /* Everything the exported tasks wrote back folded into the design's own
     state, so a writeback that never happened moves the total. */
  add_delta(lo + hi + acc);

  *verdict = checks;
  return 0;
}
