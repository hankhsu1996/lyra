/* Reads the time unit and precision of no object through VPI and through DPI,
   and counts every answer that is not the one stated here. */
#include <stddef.h>
#include <sv_vpi_user.h>
#include <svdpi.h>

static int Expect(const char* what, int got, int expected) {
  if (got == expected) return 0;
  vpi_printf("difference: %s is %d, expected %d\n", what, got, expected);
  return 1;
}

int inspect(void) {
  int differences = 0;
  differences += Expect(
      "vpi_get(vpiTimePrecision, NULL)", (int)vpi_get(vpiTimePrecision, NULL),
      -13);
  differences += Expect(
      "vpi_get(vpiTimeUnit, NULL)", (int)vpi_get(vpiTimeUnit, NULL), -13);
  int32_t unit = 0;
  int32_t precision = 0;
  if (svGetTimeUnit(NULL, &unit) != 0 ||
      svGetTimePrecision(NULL, &precision) != 0) {
    vpi_printf("difference: a DPI time query of no scope failed\n");
    return differences + 1;
  }
  differences += Expect("svGetTimeUnit(NULL)", unit, -13);
  differences += Expect("svGetTimePrecision(NULL)", precision, -13);
  return differences;
}
