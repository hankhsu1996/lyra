/* svGetCallerInfo is handed places already holding something, so whether it
   wrote them where it answers false shows (LRM Annex H.9.3). An answer of
   false is handed back as line 0, which no line of a source file is. */
#include <svdpi.h>

static const char untouched[] = "untouched";

int caller_line(const char** file) {
  const char* answered_file = untouched;
  int answered_line = -7;
  if (svGetCallerInfo(&answered_file, &answered_line)) {
    *file = answered_file;
    return answered_line;
  }
  *file = answered_file == untouched && answered_line == -7
              ? untouched
              : "written while answering false";
  return 0;
}

/* Declared by the standard's mapping of the export (LRM 35.5.6). */
extern int inner(void);

/* Asked after the export, and the import it calls, have returned. */
int line_after_inner(const char** file) {
  inner();
  return caller_line(file);
}
