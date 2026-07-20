#include <string.h>

/* Context DPI-C surface (LRM 35.5.3, Annex H). Declared here rather than via
   svdpi.h so the link input needs no extra include path; the runtime provides
   the definitions. */
typedef void* svScope;
extern svScope svGetScope(void);
extern const char* svGetNameFromScope(svScope scope);
extern svScope svGetScopeFromName(const char* name);
extern svScope svSetScope(svScope scope);
extern int svPutUserData(svScope scope, void* key, void* data);
extern void* svGetUserData(svScope scope, void* key);

typedef struct {
  int type;
  unsigned int high;
  unsigned int low;
  double real;
} svTimeVal;
extern int svGetTime(svScope scope, svTimeVal* t);
extern int svGetTimeUnit(svScope scope, int* time_unit);
extern int svGetTimePrecision(svScope scope, int* time_precision);

/* A context import observes the instantiated scope of its declaration. */
int scope_ends_with(const char* suffix) {
  const char* name = svGetNameFromScope(svGetScope());
  if (name == 0) {
    return 0;
  }
  size_t name_len = strlen(name);
  size_t suffix_len = strlen(suffix);
  return name_len >= suffix_len &&
                 strcmp(name + (name_len - suffix_len), suffix) == 0
             ? 1
             : 0;
}

/* svGetScopeFromName round-trips the current scope, and svSetScope reports the
   previous scope and installs the new one. */
int setscope_roundtrip(void) {
  svScope orig = svGetScope();
  svScope by_name = svGetScopeFromName(svGetNameFromScope(orig));
  if (by_name != orig) {
    return 0;
  }
  svScope prev = svSetScope(by_name);
  if (prev != orig) {
    return 0;
  }
  if (svGetScope() != by_name) {
    return 0;
  }
  svSetScope(orig);
  return 1;
}

/* svdpi convention: a unique userKey is the address of a user C object. */
static const int udata_key = 0;

int userdata_roundtrip(void) {
  svScope s = svGetScope();
  void* key = (void*)&udata_key;
  if (svPutUserData(s, key, (void*)0x1234) != 0) {
    return 0;
  }
  return svGetUserData(s, key) == (void*)0x1234 ? 1 : 0;
}

/* The `1ns / 1ps` timescale gives unit -9 and precision -12. At time #5 the
   current time is 5 ns: scaled to the scope's ns unit it is 5, and to the
   simulation-level ps precision (null scope) it is 5000. */
int check_time(void) {
  svScope s = svGetScope();
  int unit = 0;
  int precision = 0;
  if (svGetTimeUnit(s, &unit) != 0 || unit != -9) {
    return 0;
  }
  if (svGetTimePrecision(s, &precision) != 0 || precision != -12) {
    return 0;
  }
  svTimeVal scoped;
  if (svGetTime(s, &scoped) != 0 || scoped.low != 5 || scoped.high != 0) {
    return 0;
  }
  svTimeVal sim;
  if (svGetTime(0, &sim) != 0 || sim.low != 5000) {
    return 0;
  }
  return 1;
}
