/* Answers 1 where `vpi_mcd_close` left the whole descriptor open and reported
   it, 0 where it closed it, or nothing, without an error, and -1 otherwise. */
#include <stddef.h>
#include <vpi_user.h>

int close_and_check(int descriptor) {
  const PLI_UINT32 left = vpi_mcd_close((PLI_UINT32)descriptor);
  const PLI_INT32 level = vpi_chk_error(NULL);
  if (left == (PLI_UINT32)descriptor && left != 0 && level != 0) return 1;
  if (left == 0 && level == 0) return 0;
  return -1;
}
