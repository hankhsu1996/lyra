/* Places two error callbacks, each keeping what `vpi_chk_error` says of the
   error it is called for and then calling a routine, which replaces it. */
#include <stddef.h>
#include <sv_vpi_user.h>

typedef struct {
  int calls;
  PLI_INT32 level;
  PLI_INT32 line;
  int later_slot_placed;
} Seen;

static Seen* Callbacks(void) {
  static Seen seen[2];
  return seen;
}

static PLI_INT32 Failed(p_cb_data data) {
  if (data->reason != cbError) return 0;
  Seen* seen = (Seen*)data->user_data;
  s_vpi_error_info info;
  ++seen->calls;
  seen->level = vpi_chk_error(&info);
  seen->line = seen->level == 0 ? 0 : info.line;
  /* The run is ending by the error, so no later slot will run. */
  s_vpi_time later = {.type = vpiSimTime, .high = 0, .low = 5};
  s_cb_data after = {.reason = cbAfterDelay, .cb_rtn = Failed, .time = &later};
  if (vpi_register_cb(&after) != NULL) seen->later_slot_placed = 1;
  return 0;
}

int place(void) {
  for (int which = 0; which < 2; ++which) {
    s_cb_data data = {
        .reason = cbError,
        .cb_rtn = Failed,
        .user_data = (PLI_BYTE8*)&Callbacks()[which]};
    if (vpi_register_cb(&data) == NULL) return 1;
  }
  return 0;
}

int calls(int which) {
  return Callbacks()[which].calls;
}
int level(int which) {
  return Callbacks()[which].level;
}
int line(int which) {
  return Callbacks()[which].line;
}
int later_slot_placed(int which) {
  return Callbacks()[which].later_slot_placed;
}
