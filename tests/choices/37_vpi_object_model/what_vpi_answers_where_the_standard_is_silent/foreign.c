/* Asks `main.sv`'s scopes what the standard leaves to the tool, and counts
   every answer that is not Lyra's. */
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sv_vpi_user.h>

static void Differ(int* d, const char* what) {
  vpi_printf("difference: %s\n", what);
  ++*d;
}

static void ExpectInt(int* d, PLI_INT32 got, PLI_INT32 want, const char* what) {
  if (got != want) {
    char line[256];
    snprintf(
        line, sizeof line, "%s is %d, expected %d", what, (int)got, (int)want);
    Differ(d, line);
  }
}

static void ExpectStr(
    int* d, const char* got, const char* want, const char* what) {
  if (want == NULL ? got != NULL : got == NULL || strcmp(got, want) != 0) {
    char line[256];
    snprintf(
        line, sizeof line, "%s is '%s', expected '%s'", what,
        got == NULL ? "(null)" : got, want == NULL ? "(null)" : want);
    Differ(d, line);
  }
}

static void ExpectError(int* d, const char* what) {
  if (vpi_chk_error(NULL) != vpiError) {
    char line[256];
    snprintf(line, sizeof line, "%s reported no error", what);
    Differ(d, line);
  }
}

/* The initial block's own `begin` declares nothing, so it is no scope, and the
   two scopes inside it are Top's. A block named inside each answers the
   names of the named scopes around it as its full name, with none for the
   unnamed one, as `%m` prints it. */
static void Unnamed(int* d, vpiHandle top) {
  int unnamed = 0;
  int named_inside = 0;
  vpiHandle it = vpi_iterate(vpiInternalScope, top);
  for (vpiHandle h = it == NULL ? NULL : vpi_scan(it); h != NULL;
       h = vpi_scan(it)) {
    const PLI_INT32 type = vpi_get(vpiType, h);
    if (type != vpiBegin && type != vpiFor) continue;
    ++unnamed;
    ExpectStr(d, vpi_get_str(vpiName, h), NULL, "an unnamed scope's name");
    ExpectStr(
        d, vpi_get_str(vpiFullName, h), NULL, "an unnamed scope's full name");
    const char* inner = type == vpiBegin ? "nested" : "in_for";
    vpiHandle named = vpi_handle_by_name((PLI_BYTE8*)inner, h);
    if (named == NULL) continue;
    ++named_inside;
    char full[64];
    snprintf(full, sizeof full, "Top.%s", inner);
    ExpectStr(d, vpi_get_str(vpiName, named), inner, "an inner block's name");
    ExpectStr(
        d, vpi_get_str(vpiFullName, named), full, "an inner block's full name");
  }
  ExpectInt(d, unnamed, 2, "the unnamed scopes inside the initial block");
  ExpectInt(d, named_inside, 2, "the blocks named inside an unnamed one");
  if (vpi_handle_by_name((PLI_BYTE8*)"Top.nested", NULL) != NULL)
    Differ(d, "Top.nested reaches a block inside an unnamed one");
}

static void Internal(int* d, vpiHandle top) {
  int instances = 0;
  int generated = 0;
  int arrays = 0;
  vpiHandle it = vpi_iterate(vpiInternalScope, top);
  for (vpiHandle h = it == NULL ? NULL : vpi_scan(it); h != NULL;
       h = vpi_scan(it)) {
    switch (vpi_get(vpiType, h)) {
      case vpiModule:
        ++instances;
        break;
      case vpiGenScope:
        ++generated;
        break;
      case vpiModuleArray:
      case vpiGenScopeArray:
        ++arrays;
        break;
      default:
        break;
    }
  }
  ExpectInt(d, instances, 4, "the instances among Top's internal scopes");
  ExpectInt(d, generated, 2, "the generate scopes among them");
  ExpectInt(d, arrays, 0, "the arrays among them");
}

int walk_plus(void) {
  return 0;
}

/* Two scopes declared under escaped identifiers, an import and a clocking
   block, answer the names a hierarchical name writes for them. */
static void Escaped(int* d, vpiHandle top) {
  const char* names[] = {"\\walk+ ", "\\cb+ "};
  for (size_t i = 0; i < sizeof names / sizeof names[0]; ++i) {
    vpiHandle h = vpi_handle_by_name((PLI_BYTE8*)names[i], top);
    if (h == NULL) {
      char line[64];
      snprintf(line, sizeof line, "%s is not reached", names[i]);
      Differ(d, line);
      continue;
    }
    ExpectStr(d, vpi_get_str(vpiName, h), names[i], "an escaped scope's name");
    char full[64];
    snprintf(full, sizeof full, "Top.%s", names[i]);
    ExpectStr(
        d, vpi_get_str(vpiFullName, h), full, "an escaped scope's full name");
  }
}

int ask(void) {
  int d = 0;
  vpiHandle top = vpi_handle_by_name((PLI_BYTE8*)"Top", NULL);
  if (top == NULL) {
    Differ(&d, "Top is not reached");
    return d;
  }
  Unnamed(&d, top);
  Internal(&d, top);
  Escaped(&d, top);

  ExpectInt(
      &d, vpi_get(vpiDefDelayMode, top), vpiDelayModeNone, "Top's delay mode");
  ExpectInt(
      &d, vpi_get(vpiDefDecayTime, top), vpiUndefined, "Top's decay time");
  if (vpi_chk_error(NULL) != 0) Differ(&d, "the decay time reported an error");
  ExpectStr(&d, vpi_get_str(vpiConfig, top), NULL, "Top's configuration");
  ExpectStr(&d, vpi_get_str(vpiCell, top), "Top", "Top's cell");
  ExpectStr(&d, vpi_get_str(vpiLibrary, top), "work", "Top's library");
  vpiHandle inner = vpi_handle_by_name((PLI_BYTE8*)"Top.outer.inner", NULL);
  if (inner == NULL) {
    Differ(&d, "Top.outer.inner is not reached");
  } else {
    ExpectStr(&d, vpi_get_str(vpiCell, inner), "Outer", "inner's cell");
  }

  if (vpi_handle_by_name((PLI_BYTE8*)"std", NULL) != NULL)
    Differ(&d, "std reaches a package");
  vpiHandle pkg = vpi_handle_by_name((PLI_BYTE8*)"P", NULL);
  if (pkg == NULL || vpi_get(vpiType, pkg) != vpiPackage)
    Differ(&d, "P does not reach the package");

  if (vpi_iterate(vpiNet, top) != NULL) Differ(&d, "Top's nets are reached");
  ExpectError(&d, "asking for Top's nets");
  ExpectInt(&d, vpi_get(vpiSize, top), vpiUndefined, "Top's size");
  ExpectError(&d, "asking for Top's size");
  return d;
}
