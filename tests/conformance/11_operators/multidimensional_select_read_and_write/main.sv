// An access to a multidimensional packed array supplies an index for one
// dimension at a time, the leftmost declared dimension first, and a select
// within the element it reaches is written by chaining a further select onto
// it, however many dimensions deep the array is. The index may be a run-time
// value. The same expression addresses the element as an operand and as an
// assignment target, and a write through one leaves every bit outside it
// unchanged. Read whole, the array is the vector whose most significant part
// is its highest element, taken at each dimension in turn (LRM 11.5.2,
// 11.5.1, 7.4.4).
module Top;
  bit [7:0] low_element;
  bit [7:0] high_element;
  bit [7:0] element_by_variable;
  bit [7:0] element_by_variable_at_zero;
  bit [3:0] part_within_element;
  bit bit_within_element;
  bit [15:0] pair_as_vector;
  bit [31:0] cube_as_vector;
  bit [7:0] cube_element;
  bit [3:0] cube_nibble;
  bit [15:0] after_element_writes;
  bit [15:0] after_part_write;
  bit [15:0] after_bit_set;
  bit [15:0] after_bit_clear;
  bit [15:0] after_variable_index_write;
  bit [15:0] after_variable_base_write;
  bit [31:0] cube_after_nibble_write;

  initial begin
    bit [1:0][7:0] pair;
    bit [1:0][1:0][7:0] cube;
    bit [1:0][7:0] target;
    int index;
    int base;

    pair[0] = 8'hAB;
    pair[1] = 8'hCD;
    low_element = pair[0];
    high_element = pair[1];
    pair_as_vector = pair;
    index = 1;
    element_by_variable = pair[index];
    index = 0;
    element_by_variable_at_zero = pair[index];
    base = 4;
    part_within_element = pair[1][base +: 4];
    bit_within_element = pair[1][0];

    cube[0][0] = 8'h11;
    cube[0][1] = 8'h22;
    cube[1][0] = 8'h33;
    cube[1][1] = 8'h44;
    cube_as_vector = cube;
    cube_element = cube[1][0];
    cube_nibble = cube[1][0][3:0];
    cube[1][0][3:0] = 4'hE;
    cube_after_nibble_write = cube;

    // One array written through in turn, so each check also says that the
    // bits the write did not name kept their values.
    target[0] = 8'hCD;
    target[1] = 8'hAB;
    after_element_writes = target;
    target[0][4 +: 4] = 4'h0;
    after_part_write = target;
    target[0][1] = 1'b1;
    after_bit_set = target;
    target[0][0] = 1'b0;
    after_bit_clear = target;
    index = 1;
    target[index] = 8'h5A;
    after_variable_index_write = target;
    base = 4;
    target[1][base +: 4] = 4'hF;
    after_variable_base_write = target;
  end

  final begin
    if (low_element !== 8'hAB)
      $fatal(1, "pair[0] was %h, expected ab", low_element);
    if (high_element !== 8'hCD)
      $fatal(1, "pair[1] was %h, expected cd", high_element);
    if (element_by_variable !== 8'hCD)
      $fatal(1, "pair[index] was %h, expected cd", element_by_variable);
    if (element_by_variable_at_zero !== 8'hAB)
      $fatal(1, "pair[index] at 0 was %h, expected ab",
             element_by_variable_at_zero);
    if (pair_as_vector !== 16'hCDAB)
      $fatal(1, "pair read whole was %h, expected cdab", pair_as_vector);
    if (part_within_element !== 4'hC)
      $fatal(1, "pair[1][4+:4] was %h, expected c", part_within_element);
    if (bit_within_element !== 1'b1)
      $fatal(1, "pair[1][0] was %b, expected 1", bit_within_element);
    if (cube_as_vector !== 32'h44332211)
      $fatal(1, "cube read whole was %h, expected 44332211", cube_as_vector);
    if (cube_element !== 8'h33)
      $fatal(1, "cube[1][0] was %h, expected 33", cube_element);
    if (cube_nibble !== 4'h3)
      $fatal(1, "cube[1][0][3:0] was %h, expected 3", cube_nibble);
    if (cube_after_nibble_write !== 32'h443E2211)
      $fatal(1, "cube after the nibble write was %h, expected 443e2211",
             cube_after_nibble_write);
    if (after_element_writes !== 16'hABCD)
      $fatal(1, "after the element writes target was %h, expected abcd",
             after_element_writes);
    if (after_part_write !== 16'hAB0D)
      $fatal(1, "after the part write target was %h, expected ab0d",
             after_part_write);
    if (after_bit_set !== 16'hAB0F)
      $fatal(1, "after setting a bit target was %h, expected ab0f",
             after_bit_set);
    if (after_bit_clear !== 16'hAB0E)
      $fatal(1, "after clearing a bit target was %h, expected ab0e",
             after_bit_clear);
    if (after_variable_index_write !== 16'h5A0E)
      $fatal(1, "after target[index] target was %h, expected 5a0e",
             after_variable_index_write);
    if (after_variable_base_write !== 16'hFA0E)
      $fatal(1, "after target[1][base+:4] target was %h, expected fa0e",
             after_variable_base_write);
    $display("All checks passed");
  end
endmodule
