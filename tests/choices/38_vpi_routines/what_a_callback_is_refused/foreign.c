/* Asks for callbacks Lyra refuses, counting each that is made or that reports
   no error. */
#include <signal.h>
#include <stddef.h>
#include <sv_vpi_user.h>

static int* Differences(void) {
  static int count;
  return &count;
}

static PLI_INT32 Never(p_cb_data data) {
  vpi_printf(
      "difference: a refused callback was called, reason %d\n",
      (int)data->reason);
  ++*Differences();
  return 0;
}

static void ExpectRefused(s_cb_data* data, const char* what) {
  data->cb_rtn = Never;
  if (vpi_register_cb(data) != NULL) {
    vpi_printf("difference: %s was made\n", what);
    ++*Differences();
  }
  if (vpi_chk_error(NULL) == 0) {
    vpi_printf("difference: refusing %s reported no error\n", what);
    ++*Differences();
  }
}

static void RefuseReason(PLI_INT32 reason, const char* what) {
  s_cb_data data = {.reason = reason};
  ExpectRefused(&data, what);
}

static void RefuseOn(
    PLI_INT32 reason, const char* name, PLI_INT32 format, const char* what) {
  s_vpi_value value = {.format = format};
  s_cb_data data = {
      .reason = reason,
      .obj = vpi_handle_by_name((PLI_BYTE8*)name, NULL),
      .value = &value};
  if (data.obj == NULL) {
    vpi_printf("difference: '%s' is not reached\n", name);
    ++*Differences();
    return;
  }
  ExpectRefused(&data, what);
}

static void RefuseSignal(int signal, const char* what) {
  s_cb_data data = {.reason = cbSignal, .index = signal};
  ExpectRefused(&data, what);
}

/* Once the run has been asked to end, or is over, no later time slot runs, so
   no time is left to call a callback back at. */
static void RefuseLaterSlots(void) {
  s_vpi_time later = {.type = vpiSimTime, .high = 0, .low = 5};
  s_cb_data after = {.reason = cbAfterDelay, .time = &later};
  ExpectRefused(&after, "an after-delay callback for a later slot");
  s_cb_data next = {.reason = cbNextSimTime, .time = &later};
  ExpectRefused(&next, "a next-time callback");
}

static int* LateDifferences(void) {
  static int count = -1;
  return &count;
}

/* Runs after the design's `$finish`, in the same slot. */
static PLI_INT32 AfterFinish(p_cb_data data) {
  if (data->reason != cbReadWriteSynch) return 0;
  *Differences() = 0;
  RefuseLaterSlots();
  *LateDifferences() = *Differences();
  return 0;
}

int refuse_once_over(void) {
  *Differences() = 0;
  RefuseLaterSlots();
  return *Differences();
}

int refused_once_asked_to_end(void) {
  return *LateDifferences();
}

int refuse_all(void) {
  *Differences() = 0;
  /* Features Lyra has none of (LRM 38.36.3). */
  RefuseReason(cbStartOfSave, "a save callback");
  RefuseReason(cbStartOfRestart, "a restart callback");
  RefuseReason(cbStartOfReset, "a reset callback");
  RefuseReason(cbEnterInteractive, "an interactive callback");
  RefuseReason(cbUnresolvedSystf, "an unresolved system task callback");
  /* What it owes and has not carried out: objects VPI does not hand out yet,
     and timing checks no run carries out. */
  RefuseReason(cbStmt, "a statement callback");
  RefuseReason(cbStartOfThread, "a thread callback");
  RefuseReason(cbTchkViolation, "a timing check callback");
  /* Moments already passed. */
  RefuseReason(cbEndOfCompile, "an end-of-compile callback");
  RefuseReason(cbStartOfSimulation, "a start callback once started");
  /* Objects a reason does not apply to. */
  RefuseOn(cbValueChange, "Top.s", vpiStringVal, "a value change of a string");
  RefuseOn(
      cbValueChange, "Top.lender.lent", vpiStringVal,
      "a value change of a string a ref port is bound to");
  RefuseOn(cbValueChange, "Top.q", vpiIntVal, "a value change of a queue");
  RefuseOn(
      cbValueChange, "Top.n", vpiStrengthVal,
      "a strength change of a variable");
  RefuseOn(cbSizeChange, "Top.n", vpiIntVal, "a size change of an int");
  RefuseOn(cbDisable, "Top.n", vpiIntVal, "a disable of a variable");
  /* Signals no program can catch, or carry on past. */
  RefuseSignal(SIGKILL, "a callback on SIGKILL");
  RefuseSignal(SIGSEGV, "a callback on SIGSEGV");
  s_vpi_time at_one = {.type = vpiSimTime, .high = 0, .low = 1};
  s_cb_data after_finish = {
      .reason = cbReadWriteSynch, .cb_rtn = AfterFinish, .time = &at_one};
  if (vpi_register_cb(&after_finish) == NULL) {
    vpi_printf("difference: a read-write callback was refused\n");
    ++*Differences();
  }
  return *Differences();
}
