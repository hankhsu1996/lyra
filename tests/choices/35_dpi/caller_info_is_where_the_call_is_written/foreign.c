/* An answer of false is handed back as line 0 and an empty file, which the
   design refuses. */
#include <svdpi.h>

int caller_line(const char** file) {
  const char* answered_file = "";
  int answered_line = 0;
  if (!svGetCallerInfo(&answered_file, &answered_line)) {
    *file = "";
    return 0;
  }
  *file = answered_file;
  return answered_line;
}

/* Declared by the standard's mapping of the export (LRM 35.5.6). */
extern int inner(void);

/* Asked after the export, and the import it calls, have returned. */
int line_after_inner(const char** file) {
  inner();
  return caller_line(file);
}
