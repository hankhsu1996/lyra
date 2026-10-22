/* How many bytes written to standard output the C library still holds, not
   yet handed to the operating system. Standard output is one stream of the
   process, which the design's output and this code share. */
#include <stdio.h>
#include <stdio_ext.h>

#include "dpi.h"

int32_t unwritten_standard_output(void) {
  return (int32_t)__fpending(stdout);
}
