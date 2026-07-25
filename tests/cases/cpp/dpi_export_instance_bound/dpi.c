/* The generated ABI header brings in the design's DPI-C names and, through the
   standard header, the svdpi scope surface. A single exported symbol reaches
   whichever instance is the current DPI scope. */
#include "dpi.h"

/* Redirect the call-chain context to the named instance, then call the export so
   it runs against that instance (LRM 35.5.3). */
int read_at(const char* path) {
  svSetScope(svGetScopeFromName(path));
  return read_id();
}
