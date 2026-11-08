/* Asks for `$finish` through VPI (LRM 38.4), at level 0 so it prints nothing
   (LRM 20.2). */
#include <vpi_user.h>

void finish_from_foreign_code(void) {
  vpi_control(vpiFinish, 0);
}
