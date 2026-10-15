// A member of an unpacked union is a variable of the member's type (LRM 7.3),
// so a write to part of it -- an element of an array member, a field of a
// structure member the union holds -- changes that part of the union and
// nothing else, and a later read through the member sees it, however large the
// member is.
module Top;
  typedef struct {
    int count;
    int pad[4096];
  } padded_t;

  typedef union {
    int big[4096];
    padded_t record;
  } choice_t;

  choice_t in_array;
  choice_t in_record;
  int element_read;
  int field_read;

  initial begin
    in_array.big[3] = 21;
    element_read = in_array.big[3];
    in_record.record = '{count: 0, pad: '{default: 0}};
    in_record.record.count = 5;
    field_read = in_record.record.count;
  end

  final begin
    if (element_read !== 21)
      $fatal(1, "element_read was %0d, expected 21", element_read);
    if (field_read !== 5)
      $fatal(1, "field_read was %0d, expected 5", field_read);
    $display("All checks passed");
  end
endmodule
