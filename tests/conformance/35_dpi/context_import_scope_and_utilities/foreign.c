/* The scope, name, user-data and time utilities of the C layer, each answering
   for the scope the calling context import was declared in (LRM Annex H.9.3,
   H.13). Every answer is read back out to the design rather than judged here,
   so what the design checks is the observation and not a verdict. */
#include <stddef.h>

#include "dpi.h"

const char* scope_name(void) {
  return svGetNameFromScope(svGetScope());
}

/* One bit per step of the round trip, so a failure names the step: a scope
   recovered from its own name is the same scope, a scope named from elsewhere
   in the design is a different one, replacing the current scope reports the
   one replaced, the replacement is what is then current, and putting the
   original back restores it. What is installed is that other scope, so the
   two steps after it read a replacement that took rather than one that never
   happened. */
int32_t scope_handle_round_trip(const char* other_name) {
  svScope original = svGetScope();
  svScope by_name = svGetScopeFromName(svGetNameFromScope(original));
  svScope other = svGetScopeFromName(other_name);
  int32_t result = 0;
  if (by_name == original) {
    result |= 1;
  }
  if (other != NULL && other != original) {
    result |= 2;
  }
  if (svSetScope(other) == original) {
    result |= 4;
  }
  if (svGetScope() == other) {
    result |= 8;
  }
  svSetScope(original);
  if (svGetScope() == original) {
    result |= 16;
  }
  return result;
}

/* The recommended form of a user key is the address of a static object in the
   C code, which is unique across everything else that could run
   (LRM Annex H.9.3). */
static const int user_key = 0;

int32_t user_data_round_trip(int32_t token) {
  svScope scope = svGetScope();
  if (svPutUserData(scope, (void*)&user_key, (void*)(intptr_t)token) != 0) {
    return -1;
  }
  return (int32_t)(intptr_t)svGetUserData(scope, (void*)&user_key);
}

int32_t time_unit_power(void) {
  int32_t power = 0;
  if (svGetTimeUnit(svGetScope(), &power) != 0) {
    return 0;
  }
  return power;
}

int32_t time_precision_power(void) {
  int32_t power = 0;
  if (svGetTimePrecision(svGetScope(), &power) != 0) {
    return 0;
  }
  return power;
}

/* The time value carries the form it was asked for; simulation time is the
   integer high and low pair (LRM Annex H.13). */
int32_t time_in_scope_units(void) {
  svTimeVal now;
  now.type = sv_sim_time;
  if (svGetTime(svGetScope(), &now) != 0) {
    return -1;
  }
  if (now.type != sv_sim_time || now.high != 0) {
    return -2;
  }
  return (int32_t)now.low;
}

/* A null scope asks for the time scaled to the simulation's own unit, which
   the design's timescale makes finer than the scope's (LRM Annex H.13). */
int32_t time_in_precision_units(void) {
  svTimeVal now;
  now.type = sv_sim_time;
  if (svGetTime(NULL, &now) != 0) {
    return -1;
  }
  if (now.type != sv_sim_time || now.high != 0) {
    return -2;
  }
  return (int32_t)now.low;
}
