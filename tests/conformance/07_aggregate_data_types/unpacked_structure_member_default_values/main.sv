// A member of an unpacked structure may carry an initial assignment in the
// type declaration, and that constant expression is the member's value in
// every variable of the structure type declared without an initializer of its
// own; a member with no initial assignment takes the default initial value of
// its type instead, so a 4-state member starts as x rather than as zero. The
// initial assignment may itself be an aggregate, giving a structure or an
// array member its value. An initializer written on the variable supplies
// every member, and the type's own member initial assignments are then not
// used for it. Where the variable is declared makes no difference: a
// structure held in an array element or declared inside a procedure starts
// out the same way as one declared in a module (LRM 7.2.2, 6.8, Table 6-7,
// Table 7-1).
module Top;
  typedef struct {
    int offset;
    logic [3:0] tag;
  } inner_t;

  typedef struct {
    int with_default = 42;
    int without_default;
    logic [3:0] four_state_without_default;
    inner_t nested = '{offset: 11, tag: 4'hC};
    int filled [2] = '{20, 21};
  } entry_t;

  entry_t declared;
  entry_t initialized = '{with_default: 1, without_default: 2,
                          four_state_without_default: 4'h3,
                          nested: '{offset: 7, tag: 4'h8},
                          filled: '{30, 31}};
  entry_t in_array [2];

  int local_with_default;
  logic [3:0] local_four_state = 4'h5;
  int local_nested_offset;

  initial begin
    entry_t declared_in_procedure;
    local_with_default = declared_in_procedure.with_default;
    local_four_state = declared_in_procedure.four_state_without_default;
    local_nested_offset = declared_in_procedure.nested.offset;
  end

  final begin
    if (declared.with_default !== 42)
      $fatal(1, "declared.with_default was %0d, expected 42",
             declared.with_default);
    if (declared.without_default !== 0)
      $fatal(1, "declared.without_default was %0d, expected 0",
             declared.without_default);
    if (declared.four_state_without_default !== 4'bxxxx)
      $fatal(1, "declared.four_state_without_default was %0h, expected all x",
             declared.four_state_without_default);
    if (declared.nested.offset !== 11)
      $fatal(1, "declared.nested.offset was %0d, expected 11",
             declared.nested.offset);
    if (declared.nested.tag !== 4'hC)
      $fatal(1, "declared.nested.tag was %0h, expected c",
             declared.nested.tag);
    if (declared.filled[0] !== 20)
      $fatal(1, "declared.filled[0] was %0d, expected 20",
             declared.filled[0]);
    if (declared.filled[1] !== 21)
      $fatal(1, "declared.filled[1] was %0d, expected 21",
             declared.filled[1]);

    if (initialized.with_default !== 1)
      $fatal(1, "initialized.with_default was %0d, expected 1",
             initialized.with_default);
    if (initialized.nested.offset !== 7)
      $fatal(1, "initialized.nested.offset was %0d, expected 7",
             initialized.nested.offset);
    if (initialized.filled[0] !== 30)
      $fatal(1, "initialized.filled[0] was %0d, expected 30",
             initialized.filled[0]);

    if (in_array[1].with_default !== 42)
      $fatal(1, "in_array[1].with_default was %0d, expected 42",
             in_array[1].with_default);
    if (in_array[0].nested.tag !== 4'hC)
      $fatal(1, "in_array[0].nested.tag was %0h, expected c",
             in_array[0].nested.tag);
    if (in_array[0].four_state_without_default !== 4'bxxxx)
      $fatal(1, "in_array[0].four_state_without_default was %0h, expected x",
             in_array[0].four_state_without_default);

    if (local_with_default !== 42)
      $fatal(1, "local_with_default was %0d, expected 42",
             local_with_default);
    if (local_four_state !== 4'bxxxx)
      $fatal(1, "local_four_state was %0h, expected all x", local_four_state);
    if (local_nested_offset !== 11)
      $fatal(1, "local_nested_offset was %0d, expected 11",
             local_nested_offset);
    $display("All checks passed");
  end
endmodule
