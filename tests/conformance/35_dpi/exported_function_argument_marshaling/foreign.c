/* One driver calling every exported function back, checking each crossing and
   reporting the results as one bit each. Every actual starts at a value the
   call it is passed to does not answer with, so an argument that was never
   written reads as that rather than as an answer. */
#include "dpi.h"

int32_t drive(void) {
  int32_t verdict = 0;

  int32_t lo = -1;
  int32_t hi = -2;
  int32_t acc = 5;
  svBitVecVal wide[4];
  svLogicVecVal pattern;
  svLogic stepped_from_x = sv_z;
  svLogic stepped_from_one = sv_0;

  /* Two output scalars, each written from the input and from each other. */
  scale_pair(4, &lo, &hi);
  if (lo == 8) {
    verdict |= 1;
  }
  if (hi == 12) {
    verdict |= 2;
  }

  /* An inout scalar: the answer folds in what this side supplied. */
  accumulate(&acc);
  if (acc == 53) {
    verdict |= 4;
  }

  /* A 128-bit output fills four canonical chunks; a 128-bit input reads them
     back with a different weight each, so a chunk out of place shows. */
  wide[0] = 0;
  wide[1] = 0;
  wide[2] = 0;
  wide[3] = 0;
  fill_wide(1, wide);
  if (wide[0] == 1 && wide[1] == 2 && wide[2] == 3 && wide[3] == 4) {
    verdict |= 8;
  }
  if (weigh_wide(wide) == 30) {
    verdict |= 16;
  }

  /* A four-state output arrives in both planes, and reading the value back
     turns it into a number that no other eight-bit value produces. */
  pattern.aval = 0;
  pattern.bval = 0;
  make_pattern(1, &pattern);
  if ((pattern.aval & 0xFFu) == 0xCCu && (pattern.bval & 0xFFu) == 0x55u) {
    verdict |= 32;
  }
  if (classify(&pattern) == 29298) {
    verdict |= 64;
  }

  /* A four-state scalar crosses in a register on the way in and through a
     pointer to one on the way out. Two calls, each starting at what the other
     one answers with and answering something the other does not, so a
     writeback that never happened or that always writes the same value
     shows. */
  step_logic(sv_x, &stepped_from_x);
  step_logic(sv_1, &stepped_from_one);
  if (stepped_from_x == sv_0) {
    verdict |= 128;
  }
  if (stepped_from_one == sv_z) {
    verdict |= 256;
  }

  return verdict;
}
