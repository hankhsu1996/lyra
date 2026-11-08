/* Asks for `$finish` through VPI from an imported task (LRM 38.4), which
   answers 0 as a task that was not disabled does (LRM 35.9), and hands what
   `vpi_control` answered to the design. */
#include <vpi_user.h>

/* Defined by the design, which keeps what it is handed (LRM 35.5.6). */
void keep_what_vpi_control_answered(int answered);

int finish_from_a_task(int level) {
  keep_what_vpi_control_answered(vpi_control(vpiFinish, level));
  return 0;
}
