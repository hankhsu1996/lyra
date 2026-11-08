/* An imported task that drives the design by calling an exported task for
   ever, as a foreign testbench's main loop does. It knows nothing of the
   disable protocol, since nothing here disables it. */
#include <svdpi.h>

/* The exported subroutines called from here, declared as the design
   exports them (LRM 35.5.6, Annex H). */
int step(void);

int drive(void) {
  for (;;) {
    step();
  }
}
