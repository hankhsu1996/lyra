/* Places an end-of-simulation callback, which reads what the final procedure
   wrote and prints the sentinel where it reads it. */
#include <stddef.h>
#include <sv_vpi_user.h>

static PLI_INT32 Ended(p_cb_data data) {
  if (data->reason != cbEndOfSimulation) return 0;
  s_vpi_value value = {.format = vpiIntVal};
  vpi_get_value(vpi_handle_by_name((PLI_BYTE8*)"Top.final_ran", NULL), &value);
  if (value.value.integer == 1) vpi_printf("All checks passed\n");
  return 0;
}

int place(void) {
  s_cb_data data = {.reason = cbEndOfSimulation, .cb_rtn = Ended};
  return vpi_register_cb(&data) == NULL ? 1 : 0;
}
