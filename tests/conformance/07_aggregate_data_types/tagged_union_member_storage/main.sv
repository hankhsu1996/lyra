// A tagged union stores a member value together with a tag naming the member
// that value belongs to. The two are only ever updated together, by a tagged
// union expression naming a member, so a fresh tagged expression replaces both
// at once; and the value is read back through the member name the current tag
// holds. A member declared void carries no value, and what the variable holds
// is then the tag alone. The scheme applies to a member of any type, a
// structure, an array and a nested tagged union included, and a select or a
// further member access applies to a member as to any other variable, checked
// against the tag on the way (LRM 11.9), however large the member is beside
// the part it names; so is a method called on a member, which acts on the
// member's own value. Assigning one tagged union variable to another carries
// the tag across with the value, leaving the two independent afterwards (LRM
// 7.3.2).
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

  typedef struct {
    int count;
    int pad[4096];
  } padded_t;

  typedef int queue_t[$];

  typedef union tagged {
    queue_t entries;
    string  text;
  } held_t;

  typedef union tagged {
    int big[4096];
    padded_t record;
    int single;
  } large_t;

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
  int element_read;
  int field_read;
  int entries_size;
  int entries_found[$];
  int entries_size_after_push;
  int text_len;

  initial begin
    vint_t v;
    vint_t copy;
    vint_t source;
    vint_t holder;
    instr_t i;
    nest_t n;
    large_t in_array;
    large_t in_record;
    held_t held;

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

    in_array = tagged big '{default: 7};
    in_array.big[3] = 21;
    in_record = tagged record '{count: 5, pad: '{default: 0}};
    element_read = in_array.big[3];
    field_read = in_record.record.count;

    held = tagged entries '{4, 5, 6};
    entries_size = held.entries.size();
    // A locator finds in an unspecified order (LRM 7.12.1).
    entries_found = held.entries.find(x) with (x > 4);
    entries_found.sort();
    held.entries.push_back(7);
    entries_size_after_push = held.entries.size();
    held = tagged text "lyra";
    text_len = held.text.len();
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
    if (element_read !== 21)
      $fatal(1, "element_read was %0d, expected 21", element_read);
    if (field_read !== 5)
      $fatal(1, "field_read was %0d, expected 5", field_read);
    if (entries_size !== 3)
      $fatal(1, "entries_size was %0d, expected 3", entries_size);
    if (entries_found.size() !== 2 || entries_found[0] !== 5)
      $fatal(1, "entries_found was %p, expected '{5, 6}", entries_found);
    if (entries_size_after_push !== 4)
      $fatal(1, "entries_size_after_push was %0d, expected 4",
             entries_size_after_push);
    if (text_len !== 4)
      $fatal(1, "text_len was %0d, expected 4", text_len);
    $display("All checks passed");
  end
endmodule
