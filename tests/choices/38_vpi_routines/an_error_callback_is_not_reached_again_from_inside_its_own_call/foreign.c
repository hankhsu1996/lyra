/* A routine-error callback calls an export, which a callback may not, so the
   run reports a run-time error; a run-time-error callback then calls a routine
   that fails. Each counts its calls. */
#include <stddef.h>
#include <sv_vpi_user.h>

extern int poke(void);

typedef struct {
  int routine_errors;
  int run_time_errors;
} Calls;

static Calls* Counted(void) {
  static Calls calls;
  return &calls;
}

static PLI_INT32 OnRoutineError(p_cb_data data) {
  if (data->reason != cbPLIError) return 0;
  ++Counted()->routine_errors;
  poke();
  return 0;
}

static PLI_INT32 OnRunTimeError(p_cb_data data) {
  if (data->reason != cbError) return 0;
  ++Counted()->run_time_errors;
  /* No relation has this number, so the routine fails. */
  vpi_handle(-12345, NULL);
  return 0;
}

int place_and_fail(void) {
  s_cb_data routine = {.reason = cbPLIError, .cb_rtn = OnRoutineError};
  s_cb_data run_time = {.reason = cbError, .cb_rtn = OnRunTimeError};
  if (vpi_register_cb(&routine) == NULL || vpi_register_cb(&run_time) == NULL)
    return 1;
  vpi_handle(-12345, NULL);
  return 0;
}

int routine_errors(void) {
  return Counted()->routine_errors;
}
int run_time_errors(void) {
  return Counted()->run_time_errors;
}
