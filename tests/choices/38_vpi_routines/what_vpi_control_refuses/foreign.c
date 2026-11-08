/* Each function answers 1 where the routines behaved as the case says, and 0
   otherwise. */
#include <stddef.h>
#include <vpi_user.h>

/* Whether the last routine failed with an error, as `vpi_chk_error` reports it
   in both of its forms. */
static int FailedWithAnError(void) {
  s_vpi_error_info info;
  info.level = 0;
  info.message = NULL;
  const PLI_INT32 level = vpi_chk_error(&info);
  return level == vpiError && info.level == vpiError && info.message != NULL &&
         vpi_chk_error(NULL) == vpiError;
}

int reset_is_refused(void) {
  return vpi_control(vpiReset, 0, 0, 0) == 0 && FailedWithAnError();
}

int interactive_scope_is_refused(void) {
  return vpi_control(vpiSetInteractiveScope, NULL) == 0 && FailedWithAnError();
}

int a_check_reports_only_the_last_call(void) {
  if (vpi_control(vpiReset, 0, 0, 0) != 0 || !FailedWithAnError()) return 0;
  /* A check leaves the status as it was. */
  if (vpi_chk_error(NULL) != vpiError) return 0;
  return vpi_flush() == 0 && vpi_chk_error(NULL) == 0;
}

int finish_outside_a_context_import_is_refused(void) {
  return vpi_control(vpiFinish, 0) == 0 && FailedWithAnError();
}
