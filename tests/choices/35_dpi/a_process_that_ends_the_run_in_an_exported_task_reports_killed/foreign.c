/* An imported task whose call to an exported task ends the run. */
#include <svdpi.h>

/* Defined by the design (LRM 35.5.6). */
int step(void);

int drive(void) {
  step();
  return 0;
}
