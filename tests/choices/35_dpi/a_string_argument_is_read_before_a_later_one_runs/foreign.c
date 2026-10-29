/* The first character of a string argument, read back out to the design
   rather than judged here. */
#include <svdpi.h>

int32_t first_char(const char* a, int32_t b) {
  return b == 0 ? a[0] : -1;
}
