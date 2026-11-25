/* Drives `Top.callback_clk` from after-delay callbacks, which run in the
   Pre-Active region (LRM Table 4-1), so the clock rises at 10, 30 and 50
   before any active event of those time steps. The design's precision is
   100ps, so 10ns is 100 steps. */
#include <stddef.h>
#include <sv_vpi_user.h>

enum { kHalfPeriod = 100, kToggles = 6 };

/* The level the clock was last driven to, and how often it was toggled. */
typedef struct {
  int level;
  int toggles;
} Clock;

static PLI_INT32 Toggle(p_cb_data data);

static void ToggleAfter(Clock* clock) {
  s_vpi_time when = {.type = vpiSimTime, .high = 0, .low = kHalfPeriod};
  s_cb_data data = {
      .reason = cbAfterDelay,
      .cb_rtn = Toggle,
      .obj = NULL,
      .time = &when,
      .user_data = (PLI_BYTE8*)clock};
  vpi_register_cb(&data);
}

static PLI_INT32 Toggle(p_cb_data data) {
  Clock* clock = (Clock*)data->user_data;
  clock->level = !clock->level;
  s_vpi_value value = {.format = vpiIntVal, .value.integer = clock->level};
  vpi_put_value(
      vpi_handle_by_name((PLI_BYTE8*)"Top.callback_clk", NULL), &value, NULL,
      vpiNoDelay);
  if (++clock->toggles < kToggles) ToggleAfter(clock);
  return 0;
}

void drive_callback_clock(void) {
  static Clock clock;
  ToggleAfter(&clock);
}
