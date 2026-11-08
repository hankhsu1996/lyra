/* An imported function whose call to a package's exported function ends the
   simulation. It knows nothing of the disable protocol, since nothing here
   disables it. */
#include <svdpi.h>

/* The exported subroutines called from here, declared as the design
   exports them (LRM 35.5.6, Annex H). */
int32_t answer(int32_t);

int32_t ask(int32_t seed) {
  return answer(seed);
}
