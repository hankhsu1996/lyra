/* Reads each object in each format and compares what it reads; every answer
   that is not the one stated here is counted. */
#include <stddef.h>
#include <string.h>
#include <sv_vpi_user.h>

/* How many answers differed from the ones stated here. */
static int* Differences(void) {
  static int count;
  return &count;
}

static void Differ(const char* what, const char* of) {
  vpi_printf("difference: %s of '%s'\n", what, of);
  ++*Differences();
}

static vpiHandle Named(const char* name) {
  vpiHandle h = vpi_handle_by_name((PLI_BYTE8*)name, NULL);
  if (h == NULL) Differ("nothing is reached", name);
  return h;
}

/* The value of `name` in `format`, its format left 0 where nothing was read. */
static s_vpi_value Read(const char* name, PLI_INT32 format) {
  s_vpi_value value = {.format = format};
  vpiHandle h = Named(name);
  if (h != NULL) vpi_get_value(h, &value);
  if (vpi_chk_error(NULL) != 0) value.format = 0;
  return value;
}

static void ExpectText(const char* name, PLI_INT32 format, const char* text) {
  const s_vpi_value value = Read(name, format);
  if (value.format != format || strcmp(value.value.str, text) != 0) {
    vpi_printf(
        "difference: '%s' reads as '%s', expected '%s'\n", name,
        value.format == format ? value.value.str : "(nothing)", text);
    ++*Differences();
  }
}

static void ExpectInt(const char* name, int expected) {
  const s_vpi_value value = Read(name, vpiIntVal);
  if (value.format != vpiIntVal || value.value.integer != expected)
    Differ("the integer value", name);
}

static void ExpectScalar(const char* name, PLI_INT32 scalar) {
  const s_vpi_value value = Read(name, vpiScalarVal);
  if (value.format != vpiScalarVal || value.value.scalar != scalar)
    Differ("the scalar value", name);
}

static void ExpectOwnFormat(const char* name, PLI_INT32 format) {
  const s_vpi_value value = Read(name, vpiObjTypeVal);
  if (value.format != format) Differ("the object's own format", name);
}

static void ExpectRefused(const char* name, PLI_INT32 format) {
  s_vpi_value value = {.format = format};
  vpiHandle h = Named(name);
  if (h == NULL) return;
  vpi_get_value(h, &value);
  if (vpi_chk_error(NULL) == 0)
    Differ("a format that applies not refused", name);
}

int look(void) {
  *Differences() = 0;
  ExpectText("Top.lv", vpiBinStrVal, "10100011");
  ExpectText("Top.lv", vpiOctStrVal, "243");
  ExpectText("Top.lv", vpiHexStrVal, "a3");
  ExpectText("Top.lv", vpiDecStrVal, "163");
  ExpectInt("Top.lv", 163);
  ExpectScalar("Top.lv", vpi1);
  ExpectOwnFormat("Top.lv", vpiVectorVal);
  const s_vpi_value real_of_lv = Read("Top.lv", vpiRealVal);
  if (real_of_lv.format != vpiRealVal || real_of_lv.value.real != 163.0)
    Differ("the real value", "Top.lv");
  const s_vpi_value vector = Read("Top.lv", vpiVectorVal);
  if (vector.format != vpiVectorVal || vector.value.vector[0].aval != 0xA3 ||
      vector.value.vector[0].bval != 0) {
    Differ("the vector value", "Top.lv");
  }
  const s_vpi_value strength = Read("Top.lv", vpiStrengthVal);
  if (strength.format != vpiStrengthVal ||
      strength.value.strength[0].logic != vpi1 ||
      strength.value.strength[2].logic != vpi0 ||
      strength.value.strength[0].s0 != vpiStrongDrive ||
      strength.value.strength[0].s1 != vpiStrongDrive) {
    Differ("the strength value", "Top.lv");
  }

  ExpectText("Top.lx", vpiBinStrVal, "10xz");
  const s_vpi_value digit = Read("Top.lx", vpiHexStrVal);
  if (digit.format != vpiHexStrVal ||
      (strcmp(digit.value.str, "X") != 0 && strcmp(digit.value.str, "Z") != 0))
    Differ("the hexadecimal text", "Top.lx");
  ExpectInt("Top.lx", 8);
  ExpectScalar("Top.lx", vpiZ);
  const s_vpi_value unknown = Read("Top.lx", vpiVectorVal);
  if (unknown.format != vpiVectorVal || unknown.value.vector[0].aval != 0xA ||
      unknown.value.vector[0].bval != 0x3) {
    Differ("the vector value", "Top.lx");
  }

  ExpectOwnFormat("Top.sl", vpiScalarVal);
  ExpectScalar("Top.sl", vpi1);
  ExpectText("Top.text", vpiStringVal, "AB");
  ExpectText("Top.negative", vpiDecStrVal, "-5");
  ExpectInt("Top.negative", -5);
  ExpectOwnFormat("Top.negative", vpiIntVal);
  const s_vpi_value wide = Read("Top.wide", vpiObjTypeVal);
  const int wide_as_vector = wide.format == vpiVectorVal &&
                             wide.value.vector[0].aval == 2 &&
                             wide.value.vector[1].aval == 1;
  const int wide_as_integer =
      wide.format == vpiIntVal && wide.value.integer == 2;
  if (!wide_as_vector && !wide_as_integer)
    Differ("the value in its own format", "Top.wide");
  const s_vpi_value time = Read("Top.t", vpiObjTypeVal);
  if (time.format != vpiTimeVal || time.value.time->type != vpiSimTime ||
      time.value.time->high != 3 || time.value.time->low != 4) {
    Differ("the time value", "Top.t");
  }
  const s_vpi_value real = Read("Top.r", vpiObjTypeVal);
  if (real.format != vpiRealVal || real.value.real != 2.5)
    Differ("the real value", "Top.r");
  ExpectInt("Top.r", 3);
  ExpectOwnFormat("Top.s", vpiStringVal);
  ExpectText("Top.s", vpiStringVal, "hi");
  ExpectRefused("Top.s", vpiBinStrVal);
  ExpectRefused("Top.r", vpiHexStrVal);

  ExpectInt("Top.ps.hi", 10);
  ExpectInt("Top.ps.lo", 5);
  ExpectScalar("Top.ps[7]", vpi1);
  ExpectOwnFormat("Top.counted.count", vpiIntVal);
  ExpectInt("Top.counted.count", 7);
  ExpectOwnFormat("Top.counted.flags", vpiVectorVal);
  ExpectInt("Top.signed_rows[1]", 255);
  vpiHandle row = Named("Top.signed_rows[1]");
  if (row != NULL && vpi_get(vpiSigned, row) != 0)
    Differ("an element of a packed array is signed", "Top.signed_rows[1]");
  const s_vpi_value negative = Read("Top.wide_negative", vpiRealVal);
  if (negative.format != vpiRealVal || negative.value.real != -3.0)
    Differ("the real value", "Top.wide_negative");
  const s_vpi_value nearest = Read("Top.rounded", vpiRealVal);
  if (nearest.format != vpiRealVal ||
      (nearest.value.real != 0x1p128 + 0x1p76 && nearest.value.real != 0x1p128))
    Differ("a real neighbouring the number", "Top.rounded");
  vpiHandle pair = Named("Top.signed_pairs[1]");
  if (pair != NULL && vpi_get(vpiSigned, pair) != 0)
    Differ("an element of a packed array is signed", "Top.signed_pairs[1]");
  ExpectInt("Top.pa[1]", 0x12);
  ExpectInt("Top.pa[0].lo", 4);
  ExpectInt("Top.e", 2);
  ExpectText("Top.long_vector", vpiHexStrVal, "2affff000012345678");
  const s_vpi_value long_vector = Read("Top.long_vector", vpiVectorVal);
  if (long_vector.format != vpiVectorVal ||
      long_vector.value.vector[0].aval != 0x12345678 ||
      long_vector.value.vector[1].aval != 0xFFFF0000 ||
      long_vector.value.vector[2].aval != 0x2A) {
    Differ("the vector value", "Top.long_vector");
  }

  /* An enumeration's constant and a range's bound read as the values they
     are. */
  vpiHandle e = Named("Top.e");
  vpiHandle colours = e == NULL ? NULL : vpi_handle(vpiTypespec, e);
  vpiHandle constants =
      colours == NULL ? NULL : vpi_iterate(vpiEnumConst, colours);
  if (constants != NULL) vpi_scan(constants);
  vpiHandle green = constants == NULL ? NULL : vpi_scan(constants);
  s_vpi_value constant = {.format = vpiIntVal};
  if (green != NULL) vpi_get_value(green, &constant);
  if (green == NULL || constant.value.integer != 1)
    Differ("kGreen's value", "colour_t");
  if (constants != NULL) vpi_release_handle(constants);
  vpiHandle lv = Named("Top.lv");
  vpiHandle left = lv == NULL ? NULL : vpi_handle(vpiLeftRange, lv);
  s_vpi_value bound = {.format = vpiDecStrVal};
  if (left != NULL) vpi_get_value(left, &bound);
  if (left == NULL || strcmp(bound.value.str, "7") != 0)
    Differ("the left bound", "Top.lv");
  return *Differences();
}
