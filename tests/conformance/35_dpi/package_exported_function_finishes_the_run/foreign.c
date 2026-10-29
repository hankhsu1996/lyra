/* An imported function whose call to a package's exported function ends the
   simulation. Should the export return to this frame, the disable protocol is
   followed for whatever state it finds: acknowledged when it is set. */
#include <svdpi.h>

/* The exported subroutines called from here, declared as the design
   exports them (LRM 35.5.6, Annex H). */
int32_t answer(int32_t);

int32_t ask(int32_t seed) {
  int32_t got = answer(seed);
  if (svIsDisabledState() != 0) {
    svAckDisabledState();
  }
  return got;
}
