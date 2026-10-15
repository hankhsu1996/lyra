// A member of a tagged union is a variable of the member's type (LRM 7.3.2),
// so a select or a further member access applies to it as to any other, and
// reading through it is checked against the tag on the way (LRM 11.9) --
// however large the member is beside the part it names.
module Top;
  typedef struct {
    int count;
    int pad[4096];
  } padded_t;

  typedef union tagged {
    int big[4096];
    padded_t record;
    int single;
  } choice_t;

  choice_t in_array;
  choice_t in_record;
  int element_read;
  int field_read;

  initial begin
    in_array = tagged big '{default: 7};
    in_array.big[3] = 21;
    in_record = tagged record '{count: 5, pad: '{default: 0}};
    element_read = in_array.big[3];
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
