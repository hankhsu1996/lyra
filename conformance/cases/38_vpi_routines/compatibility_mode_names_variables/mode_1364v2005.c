/* Compiled for the 1364v2005 compatibility mode (LRM 36.12.2.1): asks what
   this mode answers of the variables of LRM Table 36-10. */
#define VPI_COMPATIBILITY_VERSION_1364v2005 1
#include <stddef.h>
#include <string.h>
#include <vpi_user.h>

/* How many answers differed from the ones stated here. */
static int* Differences(void) {
  static int count;
  return &count;
}

static void Differ(const char* what, const char* of) {
  vpi_printf("difference in 1364v2005 mode: %s of '%s'\n", what, of);
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

static int Holds(PLI_INT32 relation, const char* name) {
  vpiHandle it = vpi_iterate(relation, Named("Top.legacy"));
  int found = 0;
  for (vpiHandle next; it != NULL && (next = vpi_scan(it)) != NULL;) {
    if (strcmp(vpi_get_str(vpiName, next), name) == 0) found = 1;
  }
  return found;
}

static void ExpectHeld(
    PLI_INT32 relation, const char* name, int held, const char* relation_name) {
  if (Holds(relation, name) != held) Differ(relation_name, name);
}

int look_in_1364v2005_mode(void) {
  *Differences() = 0;
  vpiHandle mem = Named("Top.legacy.mem");
  ExpectType(mem, vpiRegArray, "vpiRegArray", "Top.legacy.mem");
  ExpectType(
      mem == NULL ? NULL : vpi_handle_by_index(mem, 0), vpiReg, "vpiReg",
      "Top.legacy.mem[0]");
  ExpectType(
      Named("Top.legacy.ia"), vpiIntegerVar, "vpiIntegerVar", "Top.legacy.ia");
  ExpectType(Named("Top.legacy.ra"), vpiRealVar, "vpiRealVar", "Top.legacy.ra");
  ExpectHeld(vpiVariables, "ia", 1, "the variables");
  ExpectHeld(vpiVariables, "ra", 1, "the variables");
  ExpectHeld(vpiVariables, "mem", 0, "the variables");
  ExpectHeld(vpiVariables, "r1", 0, "the variables");
  ExpectHeld(vpiRegArray, "mem", 1, "the reg arrays");
  ExpectHeld(vpiRegArray, "ia", 0, "the reg arrays");
  return *Differences();
}
