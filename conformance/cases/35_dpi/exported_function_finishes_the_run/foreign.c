/* An imported function whose call to an exported function ends the
   simulation. It knows nothing of the disable protocol, since nothing here
   disables it, and notes what svIsDisabledState() answers should the export
   return to it, for the design to read once the run has ended. */
#include <svdpi.h>

/* The exported subroutines called from here, declared as the design
   exports them (LRM 35.5.6, Annex H). */
int32_t answer(int32_t);

/* What svIsDisabledState() answered once the export returned here, -1 until
   it does. Kept here rather than handed to the design, which runs nothing
   once the run has ended. */
static int32_t* DisabledAfterTheExport(void) {
  static int32_t answered = -1;
  return &answered;
}

int32_t ask(int32_t seed) {
  int32_t got = answer(seed);
  *DisabledAfterTheExport() = svIsDisabledState();
  return got;
}

int32_t the_caller_was_disabled(void) {
  return *DisabledAfterTheExport();
}
