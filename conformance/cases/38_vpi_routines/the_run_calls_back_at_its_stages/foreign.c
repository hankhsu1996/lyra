/* Places the simulator action callbacks of LRM 38.36.3 a run reaches with no
   tool feature: the start and the end of the simulation, and an error in a
   VPI routine. */
#include <stddef.h>
#include <sv_vpi_user.h>

typedef struct {
  int started;
  int first_statement_had_run;
  int routine_errors;
  PLI_INT32 error_level;
  int ended;
  int finals_passed;
  int differences;
} State;

static State* Kept(void) {
  static State state;
  return &state;
}

static void Differ(const char* what) {
  vpi_printf("difference: %s\n", what);
  ++Kept()->differences;
}

static vpiHandle Place(PLI_INT32 reason, PLI_INT32 (*routine)(p_cb_data)) {
  s_cb_data data = {.reason = reason, .cb_rtn = routine};
  vpiHandle callback = vpi_register_cb(&data);
  if (callback == NULL) Differ("an action callback was refused");
  return callback;
}

static PLI_INT32 Started(p_cb_data data) {
  if (data->reason != cbStartOfSimulation)
    Differ("a start call of another reason");
  ++Kept()->started;
  s_vpi_value value = {.format = vpiIntVal};
  vpi_get_value(
      vpi_handle_by_name((PLI_BYTE8*)"Top.first_statement", NULL), &value);
  Kept()->first_statement_had_run = value.value.integer;
  return 0;
}

/* The sentinel is printed once both the final procedure has passed its checks
   and the end of the simulation has been called back, in whichever order the
   two come. */
static void PrintSentinelOnceBoth(void) {
  State* state = Kept();
  if (state->ended == 1 && state->finals_passed && state->differences == 0) {
    vpi_printf("All checks passed\n");
  }
}

static PLI_INT32 Ended(p_cb_data data) {
  if (data->reason != cbEndOfSimulation)
    Differ("an end call of another reason");
  ++Kept()->ended;
  PrintSentinelOnceBoth();
  return 0;
}

static PLI_INT32 RoutineFailed(p_cb_data data) {
  if (data->reason != cbPLIError) Differ("an error call of another reason");
  ++Kept()->routine_errors;
  s_vpi_error_info info;
  Kept()->error_level = vpi_chk_error(&info);
  return 0;
}

/* Called from a static initializer, before the simulation starts. */
int place_start(void) {
  Place(cbStartOfSimulation, Started);
  return 1;
}

int place_the_rest(void) {
  Place(cbEndOfSimulation, Ended);
  Place(cbPLIError, RoutineFailed);
  /* A routine handed no name fails (LRM 38.21). */
  if (vpi_handle_by_name(NULL, NULL) != NULL) Differ("a handle named nothing");
  return Kept()->differences;
}

int started(void) {
  return Kept()->started;
}
int first_statement_had_run(void) {
  return Kept()->first_statement_had_run;
}
int routine_errors(void) {
  return Kept()->routine_errors;
}
int error_level(void) {
  return Kept()->error_level;
}
int ended(void) {
  return Kept()->ended;
}

void finals_passed(void) {
  Kept()->finals_passed = 1;
  PrintSentinelOnceBoth();
}
