/* Places a read-write callback for the time it is now, an after-delay callback
   of no delay once the slot is under way, and a callback on SIGUSR1, which
   writes how often it was called; raises the signal; keeps what each saw. */
#include <signal.h>
#include <stddef.h>
#include <sv_vpi_user.h>

typedef struct {
  int read_write_calls;
  PLI_INT32 read_write_saw;
  int after_calls;
  PLI_UINT32 after_at;
  int signal_calls;
  PLI_UINT32 signal_at;
  int differences;
} State;

static State* Kept(void) {
  static State state;
  return &state;
}

static PLI_UINT32 Now(void) {
  s_vpi_time now = {.type = vpiSimTime};
  vpi_get_time(NULL, &now);
  return now.low;
}

static PLI_INT32 ReadWrite(p_cb_data data) {
  if (data->reason != cbReadWriteSynch) return 0;
  s_vpi_value value = {.format = vpiIntVal};
  vpi_get_value(vpi_handle_by_name((PLI_BYTE8*)"Top.x", NULL), &value);
  ++Kept()->read_write_calls;
  Kept()->read_write_saw = value.value.integer;
  return 0;
}

static PLI_INT32 After(p_cb_data data) {
  if (data->reason == cbAfterDelay) ++Kept()->after_calls;
  Kept()->after_at = Now();
  return 0;
}

static PLI_INT32 Signalled(p_cb_data data) {
  if (data->reason == cbSignal && data->index == SIGUSR1)
    ++Kept()->signal_calls;
  Kept()->signal_at = Now();
  s_vpi_value value = {
      .format = vpiIntVal, .value.integer = Kept()->signal_calls};
  vpi_put_value(
      vpi_handle_by_name((PLI_BYTE8*)"Top.poked", NULL), &value, NULL,
      vpiNoDelay);
  return 0;
}

static void Place(
    PLI_INT32 reason, PLI_INT32 (*routine)(p_cb_data), PLI_INT32 index) {
  s_vpi_time now = {.type = vpiSimTime, .high = 0, .low = 0};
  s_cb_data data = {
      .reason = reason, .cb_rtn = routine, .time = &now, .index = index};
  if (vpi_register_cb(&data) == NULL) {
    vpi_printf(
        "difference: a callback of reason %d was refused\n", (int)reason);
    ++Kept()->differences;
  }
}

int place_now(void) {
  Place(cbReadWriteSynch, ReadWrite, 0);
  Place(cbAfterDelay, After, 0);
  Place(cbSignal, Signalled, SIGUSR1);
  raise(SIGUSR1);
  return Kept()->differences;
}

int raise_again(void) {
  raise(SIGUSR1);
  return 0;
}

int read_write_calls(void) {
  return Kept()->read_write_calls;
}
int read_write_saw(void) {
  return Kept()->read_write_saw;
}
int after_calls(void) {
  return Kept()->after_calls;
}
int after_at(void) {
  return (int)Kept()->after_at;
}
int signal_calls(void) {
  return Kept()->signal_calls;
}
int signal_at(void) {
  return (int)Kept()->signal_at;
}
