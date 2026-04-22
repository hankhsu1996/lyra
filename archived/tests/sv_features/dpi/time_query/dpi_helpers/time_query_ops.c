#include "svdpi.h"

#include <stddef.h>
#include <stdint.h>

int32_t get_null_scope_time_unit(void) {
  int32_t unit;
  svGetTimeUnit(NULL, &unit);
  return unit;
}

int32_t get_null_scope_time_precision(void) {
  int32_t prec;
  svGetTimePrecision(NULL, &prec);
  return prec;
}

int32_t get_current_scope_time_unit(void) {
  int32_t unit;
  svGetTimeUnit(svGetScope(), &unit);
  return unit;
}

int32_t get_current_scope_time_precision(void) {
  int32_t prec;
  svGetTimePrecision(svGetScope(), &prec);
  return prec;
}

uint32_t get_sim_time_low(void) {
  svTimeVal tv;
  svGetTime(NULL, &tv);
  return tv.low;
}

/* Proves null scope uses simulation-level semantics, not ambient active scope.
   Call from a module with non-global timescale; null should still return
   simulation precision, not the module's unit. */
int32_t get_null_scope_time_unit_with_active_scope_present(void) {
  svScope s = svGetScope();
  (void)s;
  int32_t unit;
  svGetTimeUnit(NULL, &unit);
  return unit;
}

/* Separate-named helpers for Inner module in mixed-timescale test. */
int32_t get_inner_scope_time_unit(void) {
  int32_t unit;
  svGetTimeUnit(svGetScope(), &unit);
  return unit;
}

int32_t get_inner_scope_time_precision(void) {
  int32_t prec;
  svGetTimePrecision(svGetScope(), &prec);
  return prec;
}

/* Trigger ValidateScopeHandle with an invalid non-null scope. */
void call_time_unit_with_invalid_scope(void) {
  int32_t unit;
  svGetTimeUnit((svScope)(uintptr_t)0xDEAD, &unit);
}
