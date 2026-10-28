/* A foreign task that consumes simulation time by calling back an exported
   task that suspends (LRM 35.5.1.1). The run ends while this frame is part way
   through its loop. */
#include <svdpi.h>

/* The exported subroutines called from here, declared as the design
   exports them (LRM 35.5.6, Annex H). */
int32_t step(const char*, int32_t);

int32_t advance(const char* tag, int32_t rounds) {
  int32_t i;
  if (tag[0] == '\0') {
    return 1;
  }
  for (i = 0; i < rounds; i++) {
    if (step("a label longer than a string keeps in place, by far", i + 2) !=
        0) {
      return 1;
    }
  }
  return 0;
}
