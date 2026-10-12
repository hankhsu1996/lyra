// A reduction method answers a value of the element type, or of the with
// expression's type where one is written (LRM 7.12.3). That clause says nothing
// of an array holding no elements, so this case pins a choice of Lyra's rather
// than a requirement of the standard: over no elements a reduction answers the
// default of the type it answers (LRM 6.8 Table 6-7), x for a 4-state type and
// 0 for a 2-state one, whatever the element type is.
module Top;
  typedef logic [11:0] wide_logic_t;
  typedef logic [5:0] narrow_logic_t;
  typedef bit [5:0] narrow_bit_t;

  int nothing [$];
  byte none_dynamic [];

  int element_typed;
  logic [11:0] four_state_typed;
  shortint two_state_typed;
  logic [5:0] four_state_product;
  bit [5:0] two_state_xor;

  initial begin
    element_typed = 7;
    four_state_typed = 12'h123;
    two_state_typed = 9;
    four_state_product = 6'h15;
    two_state_xor = 6'h15;

    element_typed = nothing.sum;
    four_state_typed = nothing.sum with (wide_logic_t'(item));
    two_state_typed = nothing.sum with (shortint'(item));
    four_state_product = none_dynamic.product with (narrow_logic_t'(item));
    two_state_xor = none_dynamic.xor with (narrow_bit_t'(item));
  end

  final begin
    if (element_typed !== 0)
      $fatal(1, "element_typed was %0d, expected 0", element_typed);
    if (four_state_typed !== 12'bx)
      $fatal(1, "four_state_typed was %b, expected x", four_state_typed);
    if (two_state_typed !== 0)
      $fatal(1, "two_state_typed was %0d, expected 0", two_state_typed);
    if (four_state_product !== 6'bx)
      $fatal(1, "four_state_product was %b, expected x", four_state_product);
    if (two_state_xor !== 0)
      $fatal(1, "two_state_xor was %0d, expected 0", two_state_xor);
    $display("All checks passed");
  end
endmodule
