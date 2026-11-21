/* At the start of the simulation, asks for a read-write callback of no delay;
   at its end, prints the sentinel where that was refused. */
#include <stddef.h>
#include <sv_vpi_user.h>

static int* Placed(void) {
  static int placed = -1;
  return &placed;
}

static PLI_INT32 Started(p_cb_data data) {
  if (data->reason != cbStartOfSimulation) return 0;
  s_vpi_time now = {.type = vpiSimTime, .high = 0, .low = 0};
  s_cb_data read_write = {
      .reason = cbReadWriteSynch, .cb_rtn = Started, .time = &now};
  *Placed() = vpi_register_cb(&read_write) != NULL ? 1 : 0;
  return 0;
}

static PLI_INT32 Ended(p_cb_data data) {
  if (data->reason != cbEndOfSimulation) return 0;
  if (*Placed() == 0) vpi_printf("All checks passed\n");
  return 0;
}

int place(void) {
  s_cb_data started = {.reason = cbStartOfSimulation, .cb_rtn = Started};
  s_cb_data ended = {.reason = cbEndOfSimulation, .cb_rtn = Ended};
  if (vpi_register_cb(&started) == NULL || vpi_register_cb(&ended) == NULL)
    return 1;
  return 0;
}
