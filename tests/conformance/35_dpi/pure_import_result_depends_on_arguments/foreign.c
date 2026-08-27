/* A pure function reads nothing outside its arguments and writes nothing at
   all -- no file, no environment, no persistent data (LRM 35.5.2). Both
   arguments reach the answer, and they reach it in different decimal places,
   so exchanging them changes it. */
#include "dpi.h"

int32_t blend(int32_t a, int32_t b) {
  return (a * 100) + b;
}
