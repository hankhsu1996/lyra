/* A callback asks for the run to finish; what `vpi_control` answered is kept
   for the design to read. */
#include <stddef.h>
#include <sv_vpi_user.h>

static int* Answered(void) {
  static int answered = -1;
  return &answered;
}

static PLI_INT32 Finish(p_cb_data data) {
  *Answered() = data->reason == cbAfterDelay ? vpi_control(vpiFinish, 0) : -2;
  return 0;
}

int place_finish(void) {
  s_vpi_time delay = {.type = vpiSimTime, .high = 0, .low = 5};
  s_cb_data data = {.reason = cbAfterDelay, .cb_rtn = Finish, .time = &delay};
  return vpi_register_cb(&data) == NULL ? 0 : 1;
}

int answered(void) {
  return *Answered();
}
