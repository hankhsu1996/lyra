/* Compiled for the mode of IEEE 1800-2023 itself (LRM 36.12.2.1), which binds
   `vpi_control` to the name of the 1800-2012 mode; this is the one that asks
   for `$finish`. */
#define VPI_COMPATIBILITY_VERSION_1800v2023 1
#include <vpi_user.h>

/* Defined by the design, which keeps what it is handed (LRM 35.5.6). */
void keep_what_vpi_control_answered(int answered);

void finish_in_1800v2023_mode(void) {
  keep_what_vpi_control_answered(vpi_control(vpiFinish, 0));
}
