/* Asks each variable and the typespecs it is built from what they are, and
   counts every answer that is not the one stated here. */
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

static void ExpectInt(
    vpiHandle h, PLI_INT32 property, int expected, const char* what,
    const char* of) {
  if (h != NULL && vpi_get(property, h) != expected) Differ(what, of);
}

/* The size and the sign where each is not negative -- LRM 37.17 states no
   sign of an array -- then whether it is a scalar and a vector. */
static void Expect(
    const char* name, PLI_INT32 type, int size, int is_signed, int scalar,
    int vector) {
  vpiHandle h = Named(name);
  ExpectInt(h, vpiType, type, "the type", name);
  if (size >= 0) ExpectInt(h, vpiSize, size, "the size", name);
  if (is_signed >= 0) ExpectInt(h, vpiSigned, is_signed, "the sign", name);
  ExpectInt(h, vpiScalar, scalar, "whether it is a scalar", name);
  ExpectInt(h, vpiVector, vector, "whether it is a vector", name);
}

/* The typespec of the variable `name`. */
static vpiHandle TypespecOf(const char* name) {
  vpiHandle h = Named(name);
  return h == NULL ? NULL : vpi_handle(vpiTypespec, h);
}

static void ExpectTypespec(
    vpiHandle typespec, PLI_INT32 type, const char* typedef_name,
    const char* of) {
  if (typespec == NULL) {
    Differ("no typespec is reached", of);
    return;
  }
  ExpectInt(typespec, vpiType, type, "the typespec's type", of);
  const char* name = vpi_get_str(vpiName, typespec);
  if (typedef_name == NULL
          ? name != NULL
          : (name == NULL || strcmp(name, typedef_name) != 0)) {
    Differ("the typespec's name", of);
  }
}

/* The value of the constant a relation from `h` answers, or -1000 for none. */
static int Bound(vpiHandle h, PLI_INT32 relation) {
  vpiHandle bound = h == NULL ? NULL : vpi_handle(relation, h);
  if (bound == NULL) return -1000;
  s_vpi_value value = {.format = vpiIntVal};
  vpi_get_value(bound, &value);
  return value.value.integer;
}

static int Count(vpiHandle h, PLI_INT32 relation) {
  int count = 0;
  vpiHandle it = h == NULL ? NULL : vpi_iterate(relation, h);
  if (it == NULL) return 0;
  while (vpi_scan(it) != NULL) ++count;
  return count;
}

static void ExpectNames(
    vpiHandle h, PLI_INT32 relation, const char* first, const char* second,
    const char* of) {
  vpiHandle it = h == NULL ? NULL : vpi_iterate(relation, h);
  const char* expected[] = {first, second};
  for (int at = 0; at < 2; ++at) {
    vpiHandle next = it == NULL ? NULL : vpi_scan(it);
    const char* name = next == NULL ? NULL : vpi_get_str(vpiName, next);
    if (name == NULL || strcmp(name, expected[at]) != 0) {
      Differ("a member's name", of);
      return;
    }
  }
}

int look(void) {
  *Differences() = 0;
  Expect("Top.sl", vpiLogicVar, 1, 0, 1, 0);
  Expect("Top.lv", vpiLogicVar, 8, 0, 0, 1);
  Expect("Top.sb", vpiBitVar, 1, 0, 1, 0);
  Expect("Top.bv", vpiBitVar, 4, 0, 0, 1);
  Expect("Top.mda", vpiLogicVar, 8, 0, 0, 1);
  Expect("Top.i", vpiIntVar, 32, 1, 0, 1);
  Expect("Top.ig", vpiIntegerVar, 32, 1, 0, 1);
  Expect("Top.by", vpiByteVar, 8, 1, 0, 1);
  Expect("Top.si", vpiShortIntVar, 16, 1, 0, 1);
  Expect("Top.li", vpiLongIntVar, 64, 1, 0, 1);
  Expect("Top.t", vpiTimeVar, 64, 0, 0, 1);
  Expect("Top.r", vpiRealVar, -1, 0, 0, 0);
  Expect("Top.sr", vpiShortRealVar, -1, 0, 0, 0);
  Expect("Top.rt", vpiRealVar, -1, 0, 0, 0);
  Expect("Top.e", vpiEnumVar, 2, 0, 0, 1);
  Expect("Top.se", vpiEnumVar, 32, 1, 0, 1);
  Expect("Top.c", vpiIntVar, 32, 1, 0, 1);
  Expect("Top.ps", vpiStructVar, 8, 0, 0, 1);
  Expect("Top.pu", vpiUnionVar, 8, 0, 0, 1);
  Expect("Top.su", vpiUnionVar, 8, 0, 0, 1);
  Expect("Top.pa", vpiPackedArrayVar, 16, 0, 0, 1);
  Expect("Top.tu", vpiUnionVar, 2, 0, 0, 0);
  Expect("Top.us", vpiStructVar, 2, 0, 0, 0);
  Expect("Top.ua", vpiArrayVar, 4, -1, 0, 1);
  Expect("Top.ua2", vpiArrayVar, 6, -1, 0, 1);
  Expect("Top.s", vpiStringVar, 3, 0, 0, 0);
  Expect("Top.ch", vpiChandleVar, -1, 0, 0, 0);
  Expect("Top.cv", vpiClassVar, -1, 0, 0, 0);
  Expect("Top.da", vpiArrayVar, 3, -1, 0, 1);
  Expect("Top.q", vpiArrayVar, 2, -1, 0, 1);
  Expect("Top.aa", vpiArrayVar, 1, -1, 0, 1);
  Expect("Top.sa", vpiArrayVar, 2, -1, 0, 1);

  ExpectInt(
      Named("Top.ua"), vpiArrayType, vpiStaticArray, "the array type",
      "Top.ua");
  ExpectInt(
      Named("Top.da"), vpiArrayType, vpiDynamicArray, "the array type",
      "Top.da");
  ExpectInt(
      Named("Top.q"), vpiArrayType, vpiQueueArray, "the array type", "Top.q");
  ExpectInt(
      Named("Top.aa"), vpiArrayType, vpiAssocArray, "the array type", "Top.aa");
  ExpectInt(Named("Top.ps"), vpiPacked, 1, "whether it is packed", "Top.ps");
  ExpectInt(Named("Top.pa"), vpiPacked, 1, "whether it is packed", "Top.pa");
  ExpectInt(Named("Top.us"), vpiPacked, 0, "whether it is packed", "Top.us");
  ExpectInt(Named("Top.tu"), vpiTagged, 1, "whether it is tagged", "Top.tu");
  ExpectInt(Named("Top.pu"), vpiTagged, 0, "whether it is tagged", "Top.pu");
  ExpectInt(Named("Top.su"), vpiSoft, 1, "whether it is soft", "Top.su");
  ExpectInt(Named("Top.pu"), vpiSoft, 0, "whether it is soft", "Top.pu");

  /* A typedef of a typedef aliases it, and one of a predefined type that
     type; one of a type built where it is written aliases nothing. */
  vpiHandle paint = TypespecOf("Top.e");
  ExpectTypespec(paint, vpiEnumTypespec, "paint_t", "Top.e");
  vpiHandle colour = paint == NULL ? NULL : vpi_handle(vpiTypedefAlias, paint);
  ExpectTypespec(colour, vpiEnumTypespec, "colour_t", "paint_t's alias");
  if (colour != NULL && vpi_handle(vpiTypedefAlias, colour) != NULL) {
    Differ("an alias is answered", "colour_t");
  }
  ExpectTypespec(
      paint == NULL ? NULL : vpi_handle(vpiBaseTypespec, paint), vpiBitTypespec,
      NULL, "paint_t's base");
  if (Count(paint, vpiEnumConst) != 3) Differ("the constants", "paint_t");
  vpiHandle constants = paint == NULL ? NULL : vpi_iterate(vpiEnumConst, paint);
  vpiHandle last = NULL;
  for (vpiHandle next; constants != NULL && (next = vpi_scan(constants));) {
    last = next;
  }
  if (last == NULL || strcmp(vpi_get_str(vpiName, last), "kBlue") != 0) {
    Differ("the last constant", "paint_t");
  }
  if (last != NULL) {
    s_vpi_value value = {.format = vpiIntVal};
    vpi_get_value(last, &value);
    if (value.value.integer != 2) Differ("kBlue's value", "paint_t");
  }

  vpiHandle count = TypespecOf("Top.c");
  ExpectTypespec(count, vpiIntTypespec, "count_t", "Top.c");
  ExpectTypespec(
      count == NULL ? NULL : vpi_handle(vpiTypedefAlias, count), vpiIntTypespec,
      NULL, "count_t's alias");
  /* A class's typespec is the class, whatever typedef names it, and is
     named (LRM 37.32 details 1, 4). */
  vpiHandle item = TypespecOf("Top.named_cv");
  ExpectInt(
      item, vpiType, vpiClassTypespec, "the typespec's type", "Top.named_cv");
  if (item != NULL && vpi_get_str(vpiName, item) == NULL)
    Differ("a class typespec is named nothing", "Top.named_cv");
  if (item != NULL && vpi_handle(vpiTypedefAlias, item) != NULL)
    Differ("a class typespec aliases a typedef", "Top.named_cv");
  vpiHandle direct = TypespecOf("Top.cv");
  if (direct != NULL && vpi_get_str(vpiName, direct) == NULL)
    Differ("a class typespec is named nothing", "Top.cv");

  /* A vector's typespec is unwound one packed dimension at a time, down to
     the single bit (LRM 37.25 detail 8). */
  vpiHandle mda = TypespecOf("Top.mda");
  ExpectTypespec(mda, vpiLogicTypespec, NULL, "Top.mda");
  if (Bound(mda, vpiLeftRange) != 1 || Bound(mda, vpiRightRange) != 0) {
    Differ("the outer range", "Top.mda");
  }
  if (Count(mda, vpiRange) != 2) Differ("the ranges", "Top.mda");
  vpiHandle row = mda == NULL ? NULL : vpi_handle(vpiElemTypespec, mda);
  ExpectTypespec(row, vpiLogicTypespec, NULL, "Top.mda's element");
  if (Bound(row, vpiLeftRange) != 3) Differ("the inner range", "Top.mda");
  vpiHandle bit = row == NULL ? NULL : vpi_handle(vpiElemTypespec, row);
  ExpectTypespec(bit, vpiLogicTypespec, NULL, "Top.mda's bit");
  if (bit != NULL && (vpi_handle(vpiElemTypespec, bit) != NULL ||
                      vpi_handle(vpiLeftRange, bit) != NULL)) {
    Differ("a bit's typespec has a dimension", "Top.mda");
  }

  vpiHandle nibbles = TypespecOf("Top.ps");
  ExpectTypespec(nibbles, vpiStructTypespec, "nibbles_t", "Top.ps");
  ExpectInt(nibbles, vpiPacked, 1, "whether it is packed", "nibbles_t");
  ExpectNames(nibbles, vpiTypespecMember, "hi", "lo", "nibbles_t");
  ExpectTypespec(TypespecOf("Top.tu"), vpiUnionTypespec, "either_t", "Top.tu");
  ExpectInt(
      TypespecOf("Top.tu"), vpiTagged, 1, "whether it is tagged", "either_t");
  vpiHandle packed = TypespecOf("Top.pa");
  ExpectTypespec(packed, vpiPackedArrayTypespec, NULL, "Top.pa");
  ExpectTypespec(
      packed == NULL ? NULL : vpi_handle(vpiElemTypespec, packed),
      vpiStructTypespec, "nibbles_t", "Top.pa's element");

  /* An array typespec is unwound one unpacked dimension at a time. */
  vpiHandle grid = TypespecOf("Top.ua2");
  ExpectTypespec(grid, vpiArrayTypespec, NULL, "Top.ua2");
  ExpectInt(grid, vpiArrayType, vpiStaticArray, "the array type", "ua2's");
  vpiHandle line = grid == NULL ? NULL : vpi_handle(vpiElemTypespec, grid);
  ExpectTypespec(line, vpiArrayTypespec, NULL, "Top.ua2's element");
  if (Bound(line, vpiRightRange) != 2) Differ("the inner range", "Top.ua2");
  ExpectTypespec(
      line == NULL ? NULL : vpi_handle(vpiElemTypespec, line), vpiIntTypespec,
      NULL, "Top.ua2's leaf");
  vpiHandle dynamic = TypespecOf("Top.da");
  ExpectInt(dynamic, vpiArrayType, vpiDynamicArray, "the array type", "da's");
  if (Bound(dynamic, vpiLeftRange) != -1000) {
    Differ("a range is answered", "da's typespec");
  }
  ExpectTypespec(
      TypespecOf("Top.aa") == NULL
          ? NULL
          : vpi_handle(vpiIndexTypespec, TypespecOf("Top.aa")),
      vpiIntTypespec, NULL, "aa's index");
  ExpectTypespec(
      TypespecOf("Top.sa") == NULL
          ? NULL
          : vpi_handle(vpiIndexTypespec, TypespecOf("Top.sa")),
      vpiStringTypespec, NULL, "sa's index");

  vpiHandle pair = TypespecOf("Top.us");
  ExpectNames(pair, vpiTypespecMember, "a", "b", "pair_t");
  vpiHandle members =
      pair == NULL ? NULL : vpi_iterate(vpiTypespecMember, pair);
  if (members != NULL) vpi_scan(members);
  vpiHandle b = members == NULL ? NULL : vpi_scan(members);
  ExpectTypespec(
      b == NULL ? NULL : vpi_handle(vpiTypespec, b), vpiRealTypespec, NULL,
      "pair_t.b");
  if (members != NULL) vpi_release_handle(members);
  return *Differences();
}
