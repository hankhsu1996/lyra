/* Writes each variable and part in one format of LRM Table 38-3 each; the
   design checks what it then holds and what woke. Every routine answering
   other than as stated here is counted. */
#include <stddef.h>
#include <sv_vpi_user.h>

/* How many answers differed from the ones stated here. */
static int* Differences(void) {
  static int count;
  return &count;
}

/* The handle to an element of the dynamic array kept from one call to the
   next. */
static vpiHandle* KeptElement(void) {
  static vpiHandle kept;
  return &kept;
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

static void Write(vpiHandle h, s_vpi_value* value, const char* of) {
  if (h == NULL) return;
  vpi_put_value(h, value, NULL, vpiNoDelay);
  if (vpi_chk_error(NULL) != 0) Differ("the write was refused", of);
}

static void WriteInt(const char* name, int number) {
  s_vpi_value value = {.format = vpiIntVal, .value.integer = number};
  Write(Named(name), &value, name);
}

static void WriteText(const char* name, PLI_INT32 format, const char* text) {
  s_vpi_value value = {.format = format, .value.str = (PLI_BYTE8*)text};
  Write(Named(name), &value, name);
}

static void ExpectRefused(const char* name) {
  vpiHandle h = Named(name);
  s_vpi_value value = {.format = vpiIntVal, .value.integer = 1};
  if (h == NULL) return;
  vpi_put_value(h, &value, NULL, vpiNoDelay);
  if (vpi_chk_error(NULL) == 0) Differ("the write was not refused", name);
}

int write_all(void) {
  *Differences() = 0;
  WriteText("Top.lv", vpiHexStrVal, "21");
  s_vpi_vecval planes = {.aval = 0xC, .bval = 0x5};
  s_vpi_value vector = {.format = vpiVectorVal, .value.vector = &planes};
  Write(Named("Top.lx"), &vector, "Top.lx");
  WriteInt("Top.i", -7);
  s_vpi_value real = {.format = vpiRealVal, .value.real = 1.5};
  Write(Named("Top.r"), &real, "Top.r");
  WriteText("Top.s", vpiStringVal, "yo");
  WriteInt("Top.ps.hi", 0xC);
  WriteText("Top.ua[2]", vpiDecStrVal, "9");
  WriteInt("Top.da[1]", 4);
  WriteInt("Top.deep.row[1].m", 5);
  WriteInt("Top.over.pair.n", 8);
  WriteInt("Top.held.number", 11);
  *KeptElement() = Named("Top.da[1]");
  vpiHandle aa = Named("Top.aa");
  vpiHandle absent = aa == NULL ? NULL : vpi_handle_by_index(aa, 7);
  s_vpi_value eight = {.format = vpiIntVal, .value.integer = 8};
  Write(absent, &eight, "Top.aa[7]");
  vpiHandle unread = aa == NULL ? NULL : vpi_handle_by_index(aa, 11);
  s_vpi_value read = {.format = vpiIntVal, .value.integer = -1};
  if (unread != NULL) vpi_get_value(unread, &read);
  if (unread == NULL || read.value.integer != 0)
    Differ("an element it does not hold reads as other than 0", "Top.aa");
  s_vpi_time stamp = {.type = vpiSimTime, .high = 2, .low = 3};
  s_vpi_value time = {.format = vpiTimeVal, .value.time = &stamp};
  Write(Named("Top.t"), &time, "Top.t");
  ExpectRefused("Top.fixed");
  ExpectRefused("Top.ua");
  ExpectRefused("Top.held.fraction");
  s_vpi_strengthval strong_zero = {
      .logic = vpi0, .s0 = vpiStrongDrive, .s1 = vpiStrongDrive};
  s_vpi_value strength = {
      .format = vpiStrengthVal, .value.strength = &strong_zero};
  vpiHandle lv = Named("Top.lv");
  if (lv != NULL) {
    vpi_put_value(lv, &strength, NULL, vpiNoDelay);
    if (vpi_chk_error(NULL) == 0)
      Differ("a value with strength was put to a vector", "Top.lv");
  }
  return *Differences();
}

/* The value `i` already holds, which changes nothing; and the element the
   kept handle names now. */
int write_again(void) {
  *Differences() = 0;
  WriteInt("Top.i", -7);
  s_vpi_value element = {.format = vpiIntVal};
  if (*KeptElement() != NULL) vpi_get_value(*KeptElement(), &element);
  if (*KeptElement() == NULL || element.value.integer != 21)
    Differ(
        "a kept element reads other than what the array holds now",
        "Top.da[1]");
  return *Differences();
}
