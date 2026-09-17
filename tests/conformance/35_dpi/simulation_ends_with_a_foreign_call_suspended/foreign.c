/* A foreign task that consumes simulation time by calling back an exported
   task that suspends (LRM 35.5.1.1). The run ends while this frame is part way
   through its loop. */
#include "dpi.h"

int32_t advance(int32_t rounds) {
  int32_t i;
  for (i = 0; i < rounds; i++) {
    if (step(i + 2) != 0) {
      return 1;
    }
  }
  return 0;
}
