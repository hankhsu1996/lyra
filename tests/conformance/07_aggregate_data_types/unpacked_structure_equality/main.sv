// An unpacked structure data object is an aggregate expression, so two of
// equivalent type may be compared with the equality operators, which compare
// them member by member and yield a single bit. == and != succeed only when
// every member matches, and they yield x when x or z in a member leaves the
// relation ambiguous -- which a definite mismatch in another member settles,
// so the result stays known there. The comparison descends into a member that
// is itself a structure or an array (LRM 11.2.2, 11.4.5, 7.2).
module Top;
  typedef struct {
    int count;
    byte code;
  } entry_t;

  typedef struct {
    entry_t entry;
    int elements [2];
  } record_t;

  typedef struct {
    int count;
    logic [7:0] flags;
  } masked_t;

  entry_t entry_a, entry_b, entry_c;
  record_t record_a, record_b, record_c;
  masked_t masked_a, masked_b, masked_c;

  logic equal_entries, equal_other_entry;
  logic different_entries, different_other_entry;

  logic equal_records, equal_other_record;

  logic ambiguous_equal, ambiguous_different;
  logic settled_equal, settled_different;

  initial begin
    entry_a.count = 5;
    entry_a.code = 8'd6;
    entry_b.count = 5;
    entry_b.code = 8'd6;
    entry_c.count = 5;
    entry_c.code = 8'd7;

    equal_entries = (entry_a == entry_b);
    equal_other_entry = (entry_a == entry_c);
    different_entries = (entry_a != entry_b);
    different_other_entry = (entry_a != entry_c);

    record_a.entry = entry_a;
    record_a.elements[0] = 30;
    record_a.elements[1] = 40;
    record_b = record_a;
    record_c = record_a;
    record_c.elements[1] = 41;

    equal_records = (record_a == record_b);
    equal_other_record = (record_a == record_c);

    masked_a.count = 5;
    masked_a.flags = 8'bx;
    masked_b.count = 5;
    masked_b.flags = 8'bx;
    masked_c.count = 6;
    masked_c.flags = 8'bx;

    ambiguous_equal = (masked_a == masked_b);
    ambiguous_different = (masked_a != masked_b);
    settled_equal = (masked_a == masked_c);
    settled_different = (masked_a != masked_c);
  end

  final begin
    if (equal_entries !== 1'b1)
      $fatal(1, "equal_entries was %0h, expected 1", equal_entries);
    if (equal_other_entry !== 1'b0)
      $fatal(1, "equal_other_entry was %0h, expected 0", equal_other_entry);
    if (different_entries !== 1'b0)
      $fatal(1, "different_entries was %0h, expected 0", different_entries);
    if (different_other_entry !== 1'b1)
      $fatal(1, "different_other_entry was %0h, expected 1",
             different_other_entry);

    if (equal_records !== 1'b1)
      $fatal(1, "equal_records was %0h, expected 1", equal_records);
    if (equal_other_record !== 1'b0)
      $fatal(1, "equal_other_record was %0h, expected 0", equal_other_record);

    if (ambiguous_equal !== 1'bx)
      $fatal(1, "ambiguous_equal was %0h, expected x", ambiguous_equal);
    if (ambiguous_different !== 1'bx)
      $fatal(1, "ambiguous_different was %0h, expected x",
             ambiguous_different);
    if (settled_equal !== 1'b0)
      $fatal(1, "settled_equal was %0h, expected 0", settled_equal);
    if (settled_different !== 1'b1)
      $fatal(1, "settled_different was %0h, expected 1", settled_different);
    $display("All checks passed");
  end
endmodule
