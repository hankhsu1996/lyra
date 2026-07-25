/* Canonical DPI-C representation (LRM Annex H.10.1): aval is the value plane,
   bval the X/Z (unknown) plane; a scalar svLogic encodes sv_0=0, sv_1=1,
   sv_z=2, sv_x=3. The generated ABI header brings in both the standard layout
   and the design's exported entry points; this imported driver calls them back
   and checks the planes both ways. */
#include "dpi.h"

int drive(void) {
  svLogicVecVal v;
  v.aval = 0;
  v.bval = 0;
  make_zx(&v); /* SV 8'b1x0z_1x0z: aval=0xCC, bval=0x55 */
  int vok = (v.aval == 0xCC && v.bval == 0x55) ? 1 : 0;

  int nx = count_x(&v); /* four unknown bits -> 4 */

  svLogic b = 0;
  pass_logic(3, &b); /* x in, x out -> b == 3 */
  int lok = (b == 3) ? 1 : 0;

  return vok * 1000 + lok * 100 + nx; /* 1000 + 100 + 4 = 1104 */
}
