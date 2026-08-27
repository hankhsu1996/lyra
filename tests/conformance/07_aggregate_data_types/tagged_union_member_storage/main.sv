// A tagged union stores a member value together with a tag naming the member
// that value belongs to. The two are only ever updated together, by a tagged
// union expression naming a member, so a fresh tagged expression replaces both
// at once; and the value is read back through the member name the current tag
// holds. A member declared void carries no value, and what the variable holds
// is then the tag alone. The scheme applies to a member of any type, a
// structure and a nested tagged union included, and assigning one tagged union
// variable to another carries the tag across with the value, leaving the two
// independent afterwards (LRM 7.3.2).
module Top;
  typedef struct {
    bit [4:0] first;
    bit [4:0] second;
  } pair_t;

  typedef union tagged {
    void Invalid;
    int  Valid;
  } vint_t;

  typedef union tagged {
    pair_t    Pair;
    bit [9:0] Flat;
  } instr_t;

  typedef union tagged {
    vint_t Inner;
    int    Plain;
  } nest_t;

  int valid_read;
  int retagged_read;
  int matches_void_tag;
  int matches_void_after_retag;
  bit [4:0] struct_first;
  bit [4:0] struct_second;
  bit [9:0] flat_read;
  int nested_read;
  int copy_read;
  int copy_after_source_changed;
  int copy_carries_tag;

  initial begin
    vint_t v;
    vint_t copy;
    vint_t source;
    vint_t holder;
    instr_t i;
    nest_t n;

    v = tagged Valid (23 + 34);
    valid_read = v.Valid;

    // A second tagged expression replaces the tag and the value together.
    v = tagged Invalid;
    matches_void_tag = 0;
    if (v matches tagged Invalid) matches_void_tag = 1;

    v = tagged Valid 9;
    retagged_read = v.Valid;
    matches_void_after_retag = 1;
    if (v matches tagged Invalid) matches_void_after_retag = 1;
    else matches_void_after_retag = 0;

    i = tagged Pair '{5'd7, 5'd11};
    struct_first = i.Pair.first;
    struct_second = i.Pair.second;

    i = tagged Flat 10'h1F0;
    flat_read = i.Flat;

    n = tagged Inner (tagged Valid 3);
    nested_read = n.Inner.Valid;

    copy = v;
    copy_read = copy.Valid;
    v = tagged Valid 1;
    copy_after_source_changed = copy.Valid;

    // The target already carries a different tag, and the assignment brings
    // the source's tag over with its value.
    source = tagged Valid 7;
    holder = tagged Invalid;
    holder = source;
    copy_carries_tag = -1;
    if (holder matches tagged Valid .x) copy_carries_tag = x;
  end

  final begin
    if (valid_read !== 57)
      $fatal(1, "valid_read was %0d, expected 57", valid_read);
    if (matches_void_tag !== 1)
      $fatal(1, "matches_void_tag was %0d, expected 1", matches_void_tag);
    if (retagged_read !== 9)
      $fatal(1, "retagged_read was %0d, expected 9", retagged_read);
    if (matches_void_after_retag !== 0)
      $fatal(1, "matches_void_after_retag was %0d, expected 0",
             matches_void_after_retag);
    if (struct_first !== 5'd7)
      $fatal(1, "struct_first was %0d, expected 7", struct_first);
    if (struct_second !== 5'd11)
      $fatal(1, "struct_second was %0d, expected 11", struct_second);
    if (flat_read !== 10'h1F0)
      $fatal(1, "flat_read was %0h, expected 1f0", flat_read);
    if (nested_read !== 3)
      $fatal(1, "nested_read was %0d, expected 3", nested_read);
    if (copy_read !== 9)
      $fatal(1, "copy_read was %0d, expected 9", copy_read);
    if (copy_after_source_changed !== 9)
      $fatal(1, "copy_after_source_changed was %0d, expected 9",
             copy_after_source_changed);
    if (copy_carries_tag !== 7)
      $fatal(1, "copy_carries_tag was %0d, expected 7", copy_carries_tag);
    $display("All checks passed");
  end
endmodule
