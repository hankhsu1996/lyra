/* Compiled for the 1800v2012 compatibility mode (LRM 36.12.2.1): asks what
   this mode answers of the variables of LRM Table 36-10. */
#define VPI_COMPATIBILITY_VERSION_1800v2012 1
#include <stddef.h>
#include <string.h>
#include <sv_vpi_user.h>

/* How many answers differed from the ones stated here. */
static int* Differences(void) {
  static int count;
  return &count;
}

static void Differ(const char* what, const char* of) {
  vpi_printf("difference in 1800v2012 mode: %s of '%s'\n", what, of);
  ++*Differences();
}

static vpiHandle Named(const char* name) {
  vpiHandle h = vpi_handle_by_name((PLI_BYTE8*)name, NULL);
  if (h == NULL) Differ("nothing is reached", name);
  return h;
}

static void ExpectType(
    vpiHandle h, PLI_INT32 type, const char* type_name, const char* of) {
  if (h == NULL) return;
  if (vpi_get(vpiType, h) != type) Differ("the type", of);
  const char* name = vpi_get_str(vpiType, h);
  if (name == NULL || strcmp(name, type_name) != 0)
    Differ("the type's name", of);
}

static int Holds(const char* scope, PLI_INT32 relation, const char* name) {
  vpiHandle it = vpi_iterate(relation, Named(scope));
  int found = 0;
  for (vpiHandle next; it != NULL && (next = vpi_scan(it)) != NULL;) {
    if (strcmp(vpi_get_str(vpiName, next), name) == 0) found = 1;
  }
  return found;
}

static void ExpectHeld(
    const char* scope, PLI_INT32 relation, const char* name, int held,
    const char* relation_name) {
  if (Holds(scope, relation, name) != held) Differ(relation_name, name);
}

int look_in_1800v2012_mode(void) {
  *Differences() = 0;
  vpiHandle mem = Named("Top.legacy.mem");
  ExpectType(mem, vpiArrayVar, "vpiArrayVar", "Top.legacy.mem");
  ExpectType(
      mem == NULL ? NULL : vpi_handle_by_index(mem, 0), vpiLogicVar,
      "vpiLogicVar", "Top.legacy.mem[0]");
  ExpectType(
      Named("Top.legacy.ia"), vpiArrayVar, "vpiArrayVar", "Top.legacy.ia");
  ExpectType(
      Named("Top.legacy.ra"), vpiArrayVar, "vpiArrayVar", "Top.legacy.ra");
  ExpectHeld("Top.legacy", vpiVariables, "mem", 1, "the variables");
  ExpectHeld("Top.legacy", vpiVariables, "r1", 1, "the variables");
  ExpectHeld("Top.legacy", vpiVariables, "ia", 1, "the variables");
  ExpectHeld("Top.legacy", vpiRegArray, "mem", 1, "the reg arrays");
  ExpectHeld("Top.legacy", vpiRegArray, "ia", 1, "the reg arrays");
  ExpectHeld("Top", vpiRegArray, "counts", 1, "the reg arrays");
  ExpectHeld("Top.legacy", vpiMemory, "mem", 1, "the memories");
  return *Differences();
}
