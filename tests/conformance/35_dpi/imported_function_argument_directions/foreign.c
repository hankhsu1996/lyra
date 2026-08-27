/* Every value produced here is derived from every argument the call carried,
   and no two arguments of a call are equal, so an actual that never reached
   this side cannot yield the answer the design checks for. */
#include <stdio.h>

#include "dpi.h"

int32_t mix(int32_t high, int32_t low) {
  return (high * 1000) + low;
}

void split(int32_t combined, int32_t* high, int32_t* low) {
  *high = combined / 100;
  *low = combined % 100;
}

void fold_in(int32_t* acc, int32_t addend) {
  *acc = (*acc * 10) + addend;
}

int32_t divide(int32_t numerator, int32_t denominator, int32_t* remainder) {
  *remainder = numerator % denominator;
  return numerator / denominator;
}

int32_t char_at(const char* text, int32_t index) {
  return (int32_t)(unsigned char)text[index];
}

/* An output string does not arrive with a meaningful value; it leaves through
   the const char** the direction mode names (LRM Annex H.8.10). A code this
   side never received would select the third answer, not the second. */
void name_of(int32_t code, const char** text) {
  if (code == 1) {
    *text = "alpha";
  } else if (code == 2) {
    *text = "beta";
  } else {
    *text = "none";
  }
}

/* An inout string arrives holding a valid address and leaves holding one this
   side owns; the characters are copied out of it by the caller
   (LRM Annex H.8.10). */
void bracket(const char** text) {
  static char buffer[64];
  snprintf(buffer, sizeof buffer, "[%s]", *text);
  *text = buffer;
}

void halve(double* value) {
  *value = *value / 2.0;
}
