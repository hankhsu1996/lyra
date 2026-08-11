/* The generated ABI header declares the design's DPI-C names. */
#include "dpi.h"

/* The import is declared in the same generate scope as the export, so the
   export is callable directly and runs against that scope replica's own
   instance (LRM 35.5.3). */
int call_read(void) {
  return read_tag();
}
