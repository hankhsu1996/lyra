// $isunknown returns true when any bit of its operand is x or z, and false
// otherwise, because it is defined as the count of x and z bits being nonzero.
// The operand is a bit stream, so an unknown bit anywhere in an aggregate
// answers for the whole of it (LRM 20.9).
module Top;
  typedef struct {
    int a;
    logic [3:0] b;
  } pair_t;

  logic [3:0] value;
  pair_t pair;

  bit known;
  bit has_x;
  bit has_z;
  bit all_unknown;
  bit struct_known;
  bit struct_has_x;
  pair_t pairs[2];
  bit array_known;
  bit array_has_z;

  initial begin
    value = 4'b0101;
    known = $isunknown(value);
    value = 4'b01x1;
    has_x = $isunknown(value);
    value = 4'b01z1;
    has_z = $isunknown(value);
    value = 4'bxxxx;
    all_unknown = $isunknown(value);

    pair.a = 1;
    pair.b = 4'b0101;
    struct_known = $isunknown(pair);
    pair.b = 4'b01x1;
    struct_has_x = $isunknown(pair);

    pairs[0] = '{1, 4'b0101};
    pairs[1] = '{2, 4'b0011};
    array_known = $isunknown(pairs);
    pairs[1].b = 4'b0z11;
    array_has_z = $isunknown(pairs);
  end

  final begin
    if (known !== 1'b0)
      $fatal(1, "$isunknown of a fully known value was %b, expected 0", known);
    if (has_x !== 1'b1)
      $fatal(1, "$isunknown of a value with an x bit was %b, expected 1",
             has_x);
    if (has_z !== 1'b1)
      $fatal(1, "$isunknown of a value with a z bit was %b, expected 1",
             has_z);
    if (all_unknown !== 1'b1)
      $fatal(1, "$isunknown of an all-x value was %b, expected 1",
             all_unknown);
    if (struct_known !== 1'b0)
      $fatal(1, "$isunknown of a fully known struct was %b, expected 0",
             struct_known);
    if (struct_has_x !== 1'b1)
      $fatal(1, "$isunknown of a struct with an x bit was %b, expected 1",
             struct_has_x);
    if (array_known !== 1'b0)
      $fatal(1, "$isunknown of a fully known array of structs was %b",
             array_known);
    if (array_has_z !== 1'b1)
      $fatal(1, "$isunknown of an array of structs with a z bit was %b",
             array_has_z);
    $display("All checks passed");
  end
endmodule
