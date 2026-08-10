/* The generated ABI header declares every DPI-C name the design takes part in
   (LRM 35), so this source states none of them itself. */
#include "dpi.h"

int round_trip(int x) {
  return sv_double(x);
}

int gen_round_trip(int x) {
  return sv_triple(x);
}
