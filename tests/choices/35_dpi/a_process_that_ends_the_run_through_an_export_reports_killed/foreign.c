/* An imported function whose call to an exported function ends the run. */
#include <svdpi.h>

/* Defined by the design (LRM 35.5.6). */
void ends_the_run(void);

void call_in(void) {
  ends_the_run();
}
