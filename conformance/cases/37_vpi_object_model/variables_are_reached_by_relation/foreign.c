/* Walks each relation and compares the names of what it reaches; every
   answer that is not the one stated here is counted. */
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

static void ExpectName(vpiHandle h, const char* full, const char* what) {
  const char* got = h == NULL ? NULL : vpi_get_str(vpiFullName, h);
  if (got == NULL || strcmp(got, full) != 0) {
    vpi_printf(
        "difference: %s is '%s', expected '%s'\n", what,
        got == NULL ? "(null)" : got, full);
    ++*Differences();
  }
}

/* The full names `relation` reaches from `of`, in order; `count` of them,
   checked against the first `named` of `names`. */
static void ExpectRelated(
    const char* of, PLI_INT32 relation, int count, int named,
    const char* const* names) {
  vpiHandle from = Named(of);
  vpiHandle it = from == NULL ? NULL : vpi_iterate(relation, from);
  int at = 0;
  for (vpiHandle next; it != NULL && (next = vpi_scan(it)) != NULL; ++at) {
    if (at < named) ExpectName(next, names[at], of);
  }
  if (at != count) Differ("how many objects a relation reaches", of);
}

static int ValueOf(vpiHandle h) {
  s_vpi_value value = {.format = vpiIntVal};
  if (h == NULL) return -1000;
  vpi_get_value(h, &value);
  return value.value.integer;
}

static int Bound(const char* of, PLI_INT32 relation) {
  vpiHandle h = Named(of);
  return h == NULL ? -1000 : ValueOf(vpi_handle(relation, h));
}

static void ExpectParent(const char* of, const char* parent) {
  vpiHandle h = Named(of);
  vpiHandle up = h == NULL ? NULL : vpi_handle(vpiParent, h);
  if (parent == NULL) {
    if (up != NULL) Differ("a parent is answered", of);
    return;
  }
  ExpectName(up, parent, "the parent");
}

static int ContainsNamed(
    vpiHandle scope, PLI_INT32 relation, const char* name) {
  vpiHandle it = vpi_iterate(relation, scope);
  int found = 0;
  for (vpiHandle next; it != NULL && (next = vpi_scan(it)) != NULL;) {
    if (strcmp(vpi_get_str(vpiName, next), name) == 0) found = 1;
  }
  return found;
}

int look(void) {
  *Differences() = 0;
  vpiHandle top = Named("Top");

  /* A scope's variables include its logic variables and its arrays
     (LRM 36.12.1 entry 5), which its own relations reach as well. */
  if (!ContainsNamed(top, vpiVariables, "lv") ||
      !ContainsNamed(top, vpiVariables, "ua2") ||
      !ContainsNamed(top, vpiVariables, "differences")) {
    Differ("the variables", "Top");
  }
  if (!ContainsNamed(top, vpiReg, "lv") || ContainsNamed(top, vpiReg, "ua2")) {
    Differ("the logic variables", "Top");
  }
  if (!ContainsNamed(top, vpiRegArray, "ua2") ||
      ContainsNamed(top, vpiRegArray, "lv")) {
    Differ("the arrays", "Top");
  }

  const char* const grid[] = {
      "Top.ua2[0][0]", "Top.ua2[0][1]", "Top.ua2[0][2]", "Top.ua2[1][0]"};
  ExpectRelated("Top.ua2", vpiVarSelect, 6, 4, grid);
  const char* const halves[] = {"Top.pa[1]", "Top.pa[0]"};
  ExpectRelated("Top.pa", vpiElement, 2, 2, halves);
  const char* const nibbles[] = {"Top.pa[1].hi", "Top.pa[1].lo"};
  ExpectRelated("Top.pa[1]", vpiMember, 2, 2, nibbles);
  const char* const bits[] = {"Top.lv[7]", "Top.lv[6]"};
  ExpectRelated("Top.lv", vpiBit, 8, 2, bits);
  const char* const members[] = {"Top.spa[9][12].a", "Top.spa[9][12].bvec"};
  ExpectRelated("Top.spa[9][12]", vpiMember, 2, 2, members);
  ExpectRelated("Top.ua2", vpiRange, 2, 0, NULL);

  const char* const dynamic[] = {"Top.da[0]", "Top.da[1]", "Top.da[2]"};
  ExpectRelated("Top.da", vpiVarSelect, 3, 3, dynamic);
  const char* const keyed[] = {"Top.aa[-3]", "Top.aa[5]", "Top.aa[9]"};
  ExpectRelated("Top.aa", vpiVarSelect, 3, 3, keyed);
  vpiHandle by_byte = Named("Top.by_byte");
  vpiHandle cast = by_byte == NULL ? NULL : vpi_handle_by_index(by_byte, 300);
  vpiHandle entry = by_byte == NULL ? NULL : vpi_handle_by_index(by_byte, 44);
  vpiHandle named_entry = Named("Top.by_byte[44]");
  if (entry == NULL || named_entry == NULL ||
      vpi_compare_objects(entry, named_entry) != 1)
    Differ("an entry by index and by name differ", "Top.by_byte");
  if (cast == NULL || named_entry == NULL ||
      vpi_compare_objects(cast, named_entry) != 1 || ValueOf(cast) != 4)
    Differ(
        "an index converted into a byte names other than its entry",
        "Top.by_byte");
  ExpectName(cast, "Top.by_byte[44]", "an index converted into a byte");
  vpiHandle objects = Named("Top.by_object");
  vpiHandle by_key =
      objects == NULL ? NULL : vpi_iterate(vpiVarSelect, objects);
  vpiHandle keyed_by_object = by_key == NULL ? NULL : vpi_scan(by_key);
  if (keyed_by_object == NULL || ValueOf(keyed_by_object) != 5 ||
      vpi_scan(by_key) != NULL)
    Differ("the entries of an array indexed by a class", "Top.by_object");
  const char* const wide_keys[] = {"Top.by_wide[9223372036854775809]"};
  ExpectRelated("Top.by_wide", vpiVarSelect, 1, 1, wide_keys);
  vpiHandle wide_entry = Named("Top.by_wide[9223372036854775809]");
  vpiHandle wide_scan = vpi_iterate(vpiVarSelect, Named("Top.by_wide"));
  vpiHandle wide_seen = wide_scan == NULL ? NULL : vpi_scan(wide_scan);
  if (wide_entry == NULL || wide_seen == NULL ||
      vpi_compare_objects(wide_entry, wide_seen) != 1 ||
      ValueOf(wide_entry) != 6)
    Differ("a wide key's full name names it again", "Top.by_wide");
  if (wide_scan != NULL && wide_seen != NULL) vpi_scan(wide_scan);
  if (ValueOf(Named("Top.by_holder[3].a")) != 11)
    Differ("a member of an entry", "Top.by_holder[3].a");
  vpiHandle holders = Named("Top.by_holder");
  ExpectName(
      holders == NULL ? NULL : vpi_handle_by_index(holders, 3),
      "Top.by_holder[3]", "an entry holding a structure");
  vpiHandle handles = Named("Top.handles");
  vpiHandle handle_cast =
      handles == NULL ? NULL : vpi_handle_by_index(handles, 300);
  vpiHandle handle_named = Named("Top.handles[44]");
  if (handle_cast == NULL || handle_named == NULL ||
      vpi_compare_objects(handle_cast, handle_named) != 1)
    Differ(
        "an entry holding a class handle, by index and by name", "Top.handles");
  ExpectName(handle_cast, "Top.handles[44]", "an entry holding a class handle");
  vpiHandle huge = Named("Top.by_huge[-100000000000000000000]");
  vpiHandle huge_scan = vpi_iterate(vpiVarSelect, Named("Top.by_huge"));
  vpiHandle huge_seen = huge_scan == NULL ? NULL : vpi_scan(huge_scan);
  if (huge == NULL || huge_seen == NULL ||
      vpi_compare_objects(huge, huge_seen) != 1 || ValueOf(huge) != 12)
    Differ("a negative key wider than 64 bits by its name", "Top.by_huge");
  ExpectName(
      huge_seen, "Top.by_huge[-100000000000000000000]",
      "a negative key wider than 64 bits");
  if (huge_scan != NULL && huge_seen != NULL) vpi_scan(huge_scan);
  vpiHandle wild = Named("Top.wild");
  if (wild == NULL || vpi_get(vpiSize, wild) != 2 ||
      ValueOf(vpi_handle_by_index(wild, 300)) != 8 ||
      ValueOf(Named("Top.wild[4294967296]")) != 9)
    Differ("an entry of an array with a wildcard index", "Top.wild");
  vpiHandle all_ones = wild == NULL ? NULL : vpi_handle_by_index(wild, -1);
  vpiHandle same_bits = Named("Top.wild[4294967295]");
  if (all_ones == NULL || same_bits == NULL ||
      vpi_compare_objects(all_ones, same_bits) != 1)
    Differ("one wildcard index named two ways", "Top.wild");
  vpiHandle quoted = Named("Top.quoted");
  vpiHandle entries = quoted == NULL ? NULL : vpi_iterate(vpiVarSelect, quoted);
  int entries_seen = 0;
  for (vpiHandle entry_seen;
       entries != NULL && (entry_seen = vpi_scan(entries)) != NULL;
       ++entries_seen) {
    const char* entry_name = vpi_get_str(vpiFullName, entry_seen);
    char kept_name[64] = "";
    if (entry_name != NULL)
      strncpy(kept_name, entry_name, sizeof kept_name - 1);
    vpiHandle entry_again = vpi_handle_by_name((PLI_BYTE8*)kept_name, NULL);
    if (entry_again == NULL ||
        vpi_compare_objects(entry_seen, entry_again) != 1 ||
        ValueOf(entry_again) != ValueOf(entry_seen))
      Differ("an entry's full name names it again", kept_name);
  }
  if (entries_seen != 3)
    Differ("how many entries a relation reaches", "Top.quoted");
  vpiHandle escaped = Named("Top.quoted[\"\\say \\\"hi\\\"\\\\\"]");
  if (ValueOf(escaped) != 6)
    Differ("an escaped letter is the letter", "Top.quoted");
  const char* const texts[] = {"Top.sa[\"one\"]"};
  ExpectRelated("Top.sa", vpiVarSelect, 1, 1, texts);

  if (Bound("Top.lv", vpiLeftRange) != 7 || Bound("Top.lv", vpiRightRange) != 0)
    Differ("the bounds", "Top.lv");
  if (Bound("Top.ua2", vpiLeftRange) != 0 ||
      Bound("Top.ua2", vpiRightRange) != 1) {
    Differ("the bounds", "Top.ua2");
  }
  if (Bound("Top.da", vpiLeftRange) != 0 || Bound("Top.da", vpiRightRange) != 2)
    Differ("the bounds of what it holds", "Top.da");
  if (Bound("Top.aa", vpiLeftRange) != -1000)
    Differ("an associative array's range is not empty", "Top.aa");

  /* Indices come from the part's own outward. */
  vpiHandle cell = Named("Top.ua2[1][2]");
  vpiHandle indices = cell == NULL ? NULL : vpi_iterate(vpiIndex, cell);
  vpiHandle first = indices == NULL ? NULL : vpi_scan(indices);
  vpiHandle second = indices == NULL ? NULL : vpi_scan(indices);
  if (ValueOf(first) != 2 || ValueOf(second) != 1)
    Differ("the indices", "Top.ua2[1][2]");
  if (indices != NULL && vpi_scan(indices) != NULL)
    Differ("an index too many", "Top.ua2[1][2]");
  if (ValueOf(cell == NULL ? NULL : vpi_handle(vpiIndex, cell)) != 2)
    Differ("the index", "Top.ua2[1][2]");
  vpiHandle packed_cell = Named("Top.upa[1][0][3]");
  vpiHandle packed_indices =
      packed_cell == NULL ? NULL : vpi_iterate(vpiIndex, packed_cell);
  vpiHandle innermost =
      packed_indices == NULL ? NULL : vpi_scan(packed_indices);
  vpiHandle outer = packed_indices == NULL ? NULL : vpi_scan(packed_indices);
  if (ValueOf(innermost) != 3 || ValueOf(outer) != 0)
    Differ("the indices", "Top.upa[1][0][3]");
  if (packed_indices != NULL && vpi_scan(packed_indices) != NULL)
    Differ("an index past its parent", "Top.upa[1][0][3]");
  ExpectParent("Top.upa[1][0][3]", "Top.upa[1]");

  ExpectParent("Top.ua2[1][2]", "Top.ua2");
  ExpectParent("Top.lv[3]", "Top.lv");
  ExpectParent("Top.pa[1].hi", "Top.pa[1]");
  ExpectParent("Top.pa[1]", "Top.pa");
  ExpectParent("Top.spa[9][12].bvec[4][0]", "Top.spa[9][12].bvec[4]");
  ExpectParent("Top.spa[9][12].bvec[4]", "Top.spa[9][12].bvec");
  ExpectParent("Top.spa[9][12].bvec", "Top.spa[9][12]");
  ExpectParent("Top.spa[9][12]", "Top.spa");
  ExpectParent("Top.spa", NULL);

  vpiHandle ua2 = Named("Top.ua2");
  ExpectName(
      ua2 == NULL ? NULL : vpi_handle_by_index(ua2, 1), "Top.ua2[1]",
      "by index");
  PLI_INT32 both[] = {1, 2};
  ExpectName(
      ua2 == NULL ? NULL : vpi_handle_by_multi_index(ua2, 2, both),
      "Top.ua2[1][2]", "by indices");
  vpiHandle lv = Named("Top.lv");
  PLI_INT32 bit[] = {5};
  ExpectName(
      lv == NULL ? NULL : vpi_handle_by_multi_index(lv, 1, bit), "Top.lv[5]",
      "a bit by index");
  if (ua2 != NULL && vpi_handle_by_index(ua2, 2) != NULL)
    Differ("an index outside the range reaches something", "Top.ua2");
  vpiHandle up = Named("Top.up");
  vpiHandle down = Named("Top.down");
  vpiHandle up6 = up == NULL ? NULL : vpi_handle_by_index(up, 6);
  vpiHandle down6 = down == NULL ? NULL : vpi_handle_by_index(down, 6);
  ExpectName(up6, "Top.up[6]", "an index of an ascending range");
  ExpectName(down6, "Top.down[6]", "an index of a descending range");
  if (ValueOf(up6) != 16 || ValueOf(down6) != 26)
    Differ("the element an index reaches", "Top.up and Top.down");

  vpiHandle low_bit = Named("Top.ps[0]");
  vpiHandle member_bit = Named("Top.ps.b[7]");
  vpiHandle other_bit = Named("Top.ps.b[6]");
  if (low_bit != NULL && member_bit != NULL &&
      vpi_compare_objects(low_bit, member_bit) != 1) {
    Differ("one bit named two ways compares unequal", "Top.ps");
  }
  if (low_bit != NULL && other_bit != NULL &&
      vpi_compare_objects(low_bit, other_bit) != 0) {
    Differ("two bits compare equal", "Top.ps");
  }

  vpiHandle da = Named("Top.da");
  if (da != NULL && vpi_handle_by_index(da, 3) != NULL)
    Differ("an index past what it holds reaches something", "Top.da");
  return *Differences();
}
