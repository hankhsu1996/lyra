/* Asks for `$finish` through VPI (LRM 38.4) and hands what it was answered to
   the design. */
#include <vpi_user.h>

/* Defined by the design, which keeps what it is handed (LRM 35.5.6). */
void keep_what_vpi_control_answered(int answered);

void finish_from_foreign_code(int level) {
  keep_what_vpi_control_answered(vpi_control(vpiFinish, level));
}
