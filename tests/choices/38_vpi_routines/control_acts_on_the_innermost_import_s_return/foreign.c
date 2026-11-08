/* An import that calls an export, and an import the export calls that asks
   for `$finish` through VPI (LRM 38.4). */
#include <vpi_user.h>

/* Defined by the design (LRM 35.5.6). */
void the_export(void);
void keep_what_vpi_control_answered(int answered);

/* Nothing disabled this import, so it owes the disable protocol nothing (LRM
   35.9) and returns once the export does. */
void call_through_an_export(void) {
  the_export();
}

void finish_from_foreign_code(void) {
  keep_what_vpi_control_answered(vpi_control(vpiFinish, 0));
}
