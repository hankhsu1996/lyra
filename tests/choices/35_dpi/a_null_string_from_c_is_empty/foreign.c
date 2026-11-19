/* Each hands a null pointer over where a string crosses to SystemVerilog. */
#include <stddef.h>
#include <svdpi.h>

const char* no_text(void) {
  return NULL;
}

void no_text_out(const char** s) {
  *s = NULL;
}

void no_text_inout(const char** s) {
  *s = NULL;
}

/* The exported subroutine called from here, declared as the design exports
   it (LRM 35.5.6, Annex H). */
void take_text(const char* s);

void hand_no_text(void) {
  take_text(NULL);
}
