/* Reaches each variable and part of one by name and compares the names it
   answers and its type; every answer that is not the one stated here is
   counted. */
#include <stddef.h>
#include <string.h>
#include <sv_vpi_user.h>

/* How many answers differed from the ones stated here. */
static int* Differences(void) {
  static int count;
  return &count;
}

static void Differ(const char* what, const char* name) {
  vpi_printf("difference: %s for '%s'\n", what, name);
  ++*Differences();
}

static void ExpectString(
    vpiHandle h, PLI_INT32 property, const char* expected, const char* what) {
  const char* got = vpi_get_str(property, h);
  if (got == NULL || strcmp(got, expected) != 0) {
    vpi_printf(
        "difference: %s is '%s', expected '%s'\n", what,
        got == NULL ? "(null)" : got, expected);
    ++*Differences();
  }
}

/* `full` is the name looked up and the full name it answers. */
static vpiHandle Expect(
    const char* lookup, vpiHandle scope, const char* full, const char* name,
    const char* decompile, PLI_INT32 type) {
  vpiHandle h = vpi_handle_by_name((PLI_BYTE8*)lookup, scope);
  if (h == NULL) {
    Differ("nothing is reached", lookup);
    return NULL;
  }
  ExpectString(h, vpiFullName, full, "the full name");
  ExpectString(h, vpiName, name, "the name");
  if (decompile != NULL) {
    ExpectString(h, vpiDecompile, decompile, "the decompiled name");
  }
  if (vpi_get(vpiType, h) != type) Differ("the type differs", lookup);
  return h;
}

static void ExpectNothing(const char* lookup, vpiHandle scope) {
  if (vpi_handle_by_name((PLI_BYTE8*)lookup, scope) != NULL) {
    Differ("a handle is answered", lookup);
  }
}

int look(void) {
  *Differences() = 0;
  Expect("Top.arr1", NULL, "Top.arr1", "arr1", "arr1", vpiArrayVar);
  Expect(
      "Top.arr1[1][9]", NULL, "Top.arr1[1][9]", "arr1[1][9]", "arr1[1][9]",
      vpiBitVar);
  Expect(
      "Top.arr1[4][15][7]", NULL, "Top.arr1[4][15][7]", "arr1[4][15][7]",
      "arr1[4][15][7]", vpiVarBit);
  Expect("Top.str1", NULL, "Top.str1", "str1", "str1", vpiStructVar);
  Expect("Top.str1.i1", NULL, "Top.str1.i1", "i1", "str1.i1", vpiIntegerVar);
  Expect(
      "Top.str1.inner1.j1", NULL, "Top.str1.inner1.j1", "j1", "str1.inner1.j1",
      vpiShortIntVar);
  Expect(
      "Top.str1.vec[5]", NULL, "Top.str1.vec[5]", "vec[5]", "str1.vec[5]",
      vpiLogicVar);
  Expect("Top.word[3]", NULL, "Top.word[3]", "word[3]", "word[3]", vpiVarBit);
  Expect(
      "Top.g[1].in_block", NULL, "Top.g[1].in_block", "in_block", NULL,
      vpiIntVar);
  Expect("Top.main.kept", NULL, "Top.main.kept", "kept", NULL, vpiIntVar);
  Expect("P::counter", NULL, "P::counter", "counter", NULL, vpiIntVar);

  /* Within a scope a part is named by the way from its variable. */
  vpiHandle top = vpi_handle_by_name((PLI_BYTE8*)"Top", NULL);
  Expect(
      "str1.inner1.b1", top, "Top.str1.inner1.b1", "b1", "str1.inner1.b1",
      vpiByteVar);
  Expect("arr1[2][10]", top, "Top.arr1[2][10]", "arr1[2][10]", NULL, vpiBitVar);

  ExpectNothing("Top.word[8]", NULL);
  ExpectNothing("Top.arr1[5][9]", NULL);
  ExpectNothing("Top.str1.missing", NULL);
  ExpectNothing("Top.nothing", NULL);
  ExpectNothing("Top.str1.i1.deeper", NULL);
  return *Differences();
}
