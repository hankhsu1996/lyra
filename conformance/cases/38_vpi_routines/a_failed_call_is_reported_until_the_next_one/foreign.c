/* Each function but the first answers 1 where `vpi_chk_error` behaved as LRM
   38.2 says, and 0 otherwise. */
#include <stddef.h>
#include <vpi_user.h>

/* The level a check with a structure answered, where the structure agrees
   with it and carries a message, and -1 where it does not. */
static PLI_INT32 CheckedLevel(void) {
  s_vpi_error_info info;
  info.level = -1;
  info.message = NULL;
  const PLI_INT32 level = vpi_chk_error(&info);
  if (level == 0 || info.level != level || info.message == NULL) return -1;
  return level;
}

int what_closing_one_answered(void) {
  return (int)vpi_mcd_close(1);
}

int the_failure_is_reported(void) {
  vpi_mcd_close(1);
  return CheckedLevel() > 0;
}

int it_is_reported_again(void) {
  vpi_mcd_close(1);
  const PLI_INT32 first = CheckedLevel();
  return first > 0 && CheckedLevel() == first && vpi_chk_error(NULL) == first;
}

int the_next_call_replaces_it(void) {
  s_vpi_error_info info;
  vpi_mcd_close(1);
  if (vpi_chk_error(NULL) == 0) return 0;
  if (vpi_flush() != 0) return 0;
  info.level = -1;
  return vpi_chk_error(&info) == 0 && vpi_chk_error(NULL) == 0;
}
