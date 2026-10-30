// Prefixing an assignment pattern with a type name makes an expression whose
// data type is self-determined, so it is not confined to one side of an
// assignment-like context and yields the value a variable of that type would
// hold had it been initialized by the pattern. Over a packed type that value
// is a vector, so it may stand as an operand of a concatenation or of a cast
// (LRM 10.9).
module Top;
  typedef logic [1:0][3:0] nibbles_t;

  typedef struct packed {
    logic [3:0] high;
    logic [3:0] low;
  } pair_t;

  shortint concatenated = 16'h9999;
  logic [7:0] from_array = 8'h99;
  logic [7:0] from_structure = 8'h99;

  initial begin
    concatenated = shortint'({nibbles_t'{1, 2}, nibbles_t'{3, 4}});
    from_array = nibbles_t'{4'hA, 4'hB};
    from_structure = pair_t'{4'h7, 4'hE};
  end

  final begin
    if (concatenated !== 16'sh1234)
      $fatal(1, "the concatenated patterns were %0h, expected 1234",
             concatenated);
    if (from_array !== 8'hAB)
      $fatal(1, "the array pattern was %0h, expected ab", from_array);
    if (from_structure !== 8'h7E)
      $fatal(1, "the structure pattern was %0h, expected 7e", from_structure);
    $display("All checks passed");
  end
endmodule
