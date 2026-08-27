/* An imported task is written in C as a function returning int: the value
   reports whether the task acknowledged a disable, and none is active here
   (LRM 35.5.4, 35.9). */
#include "dpi.h"

int32_t set_pair(int32_t seed, int32_t* doubled, int32_t* next) {
  *doubled = seed * 2;
  *next = seed + 1;
  return 0;
}

int32_t accumulate(int32_t delta, int32_t* total) {
  *total = (*total * 2) + delta;
  return 0;
}

/* State this side keeps between calls, so what one call carried in is only
   readable by a later one if the value really crossed (LRM 35.5.1.4). */
static int32_t remembered;

int32_t remember(int32_t value) {
  remembered = value;
  return 0;
}

int32_t recall(int32_t* value) {
  *value = remembered;
  return 0;
}
