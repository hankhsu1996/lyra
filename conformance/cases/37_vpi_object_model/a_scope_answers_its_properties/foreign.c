/* Reads the properties of the scopes of `main.sv` and counts every answer that
   is not the one stated here. */
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
  if (got == NULL || strcmp(got, want) != 0) {
    char line[256];
    snprintf(
        line, sizeof line, "%s is '%s', expected '%s'", what,
        got == NULL ? "(null)" : got, want);
    Differ(d, line);
  }
}

static void Module(int* d, vpiHandle top, vpiHandle cell, vpiHandle slow) {
  ExpectStr(d, vpi_get_str(vpiType, top), "vpiModule", "Top's type name");
  ExpectStr(d, vpi_get_str(vpiName, top), "Top", "Top's name");
  ExpectStr(d, vpi_get_str(vpiDefName, cell), "Cell", "cell's definition");
  ExpectStr(d, vpi_get_str(vpiCell, cell), "Cell", "cell's cell");

  ExpectInt(d, vpi_get(vpiTop, top), 1, "Top's vpiTop");
  ExpectInt(d, vpi_get(vpiTop, cell), 0, "cell's vpiTop");
  ExpectInt(d, vpi_get(vpiTopModule, cell), 0, "cell's vpiTopModule");
  ExpectInt(d, vpi_get(vpiCellInstance, cell), 1, "cell's vpiCellInstance");
  ExpectInt(d, vpi_get(vpiCellInstance, top), 0, "Top's vpiCellInstance");
  ExpectInt(d, vpi_get(vpiDefNetType, cell), vpiNone, "cell's net type");
  ExpectInt(d, vpi_get(vpiDefNetType, top), vpiWire, "Top's net type");
  ExpectInt(d, vpi_get(vpiUnconnDrive, cell), vpiPull1, "cell's drive");
  ExpectInt(d, vpi_get(vpiUnconnDrive, top), vpiHighZ, "Top's drive");
  ExpectInt(d, vpi_get(vpiAutomatic, cell), 1, "cell's vpiAutomatic");
  ExpectInt(d, vpi_get(vpiAutomatic, top), 0, "Top's vpiAutomatic");
  ExpectInt(d, vpi_get(vpiIsProtected, top), 0, "Top's vpiIsProtected");
  ExpectInt(d, vpi_get(vpiProtected, top), 0, "Top's vpiProtected");
  ExpectInt(d, vpi_get(vpiArrayMember, top), 0, "Top's vpiArrayMember");
  ExpectInt(d, vpi_get(vpiTimeUnit, top), -9, "Top's time unit");
  ExpectInt(d, vpi_get(vpiTimePrecision, top), -12, "Top's precision");
  ExpectInt(d, vpi_get(vpiTimeUnit, slow), -6, "slow's time unit");
  ExpectInt(d, vpi_get(vpiTimePrecision, slow), -9, "slow's precision");
  ExpectInt(d, vpi_get(vpiTimeUnit, NULL), -12, "no object's time unit");
  ExpectInt(d, vpi_get(vpiTimePrecision, NULL), -12, "no object's precision");
}

/* The built-in package `std` is among the packages too (LRM 26.7), so `P` is
   looked for by its name. */
static void Packages(int* d) {
  int units = 0;
  int named_p = 0;
  vpiHandle it = vpi_iterate(vpiInstance, NULL);
  if (it == NULL) {
    Differ(d, "there are no instances");
    return;
  }
  for (vpiHandle h = vpi_scan(it); h != NULL; h = vpi_scan(it)) {
    if (vpi_get(vpiType, h) != vpiPackage) continue;
    if (vpi_get(vpiUnit, h) == 1) {
      ++units;
      continue;
    }
    const char* name = vpi_get_str(vpiName, h);
    if (name == NULL || strcmp(name, "P") != 0) continue;
    ++named_p;
    ExpectStr(d, vpi_get_str(vpiFullName, h), "P::", "the package's full name");
    ExpectStr(d, vpi_get_str(vpiType, h), "vpiPackage", "its type's name");
  }
  ExpectInt(d, named_p, 1, "the packages named P among the instances");
  ExpectInt(d, units, 1, "the compilation units among the instances");
}

static void Time(int* d, vpiHandle top, vpiHandle slow) {
  s_vpi_time t;
  t.type = vpiSimTime;
  t.high = 0xffffffffU;
  t.low = 0xffffffffU;
  vpi_get_time(NULL, &t);
  ExpectInt(d, (PLI_INT32)t.high, 0, "the high word of the time");
  ExpectInt(d, (PLI_INT32)t.low, 5000, "the time in the simulation's unit");
  t.type = vpiSimTime;
  t.low = 0xffffffffU;
  vpi_get_time(top, &t);
  ExpectInt(d, (PLI_INT32)t.low, 5, "the time in Top's unit");
  t.type = vpiScaledRealTime;
  t.real = -1.0;
  vpi_get_time(top, &t);
  if (t.real < 4.999999 || t.real > 5.000001)
    Differ(d, "the time in Top's unit is not 5");
  t.real = -1.0;
  vpi_get_time(slow, &t);
  if (t.real < 0.004999 || t.real > 0.005001)
    Differ(d, "the time in slow's unit is not 0.005");
}

int inspect(void) {
  int differences = 0;
  vpiHandle top = vpi_handle_by_name((PLI_BYTE8*)"Top", NULL);
  vpiHandle cell = vpi_handle_by_name((PLI_BYTE8*)"Top.c", NULL);
  vpiHandle slow = vpi_handle_by_name((PLI_BYTE8*)"Top.slow", NULL);
  if (top == NULL || cell == NULL || slow == NULL) {
    Differ(&differences, "Top, Top.c or Top.slow is not reached");
    return differences;
  }
  Module(&differences, top, cell, slow);
  Packages(&differences);
  Time(&differences, top, slow);
  return differences;
}
