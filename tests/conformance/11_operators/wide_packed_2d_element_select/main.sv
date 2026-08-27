// Access to an array supplies an index expression for each dimension, and a
// bit-select or part-select of an element is written by selecting the
// element first and then addressing within it exactly as within a vector.
// The element index may be a run-time value, and the element being wider
// than one machine word changes none of this (LRM 11.5.2, 11.5.1).
module Top;
  bit [1:0][127:0] matrix;
  int idx;
  bit [127:0] whole_element;
  bit [127:0] element_by_variable;
  bit [127:0] element_by_variable_at_zero;
  bit [11:0] element_part;
  bit [11:0] element_part_by_variable;
  bit element_bit_set;
  bit element_bit_clear;

  initial begin
    element_bit_clear = 1'b1;

    matrix[0] = 128'hF0F0F0F0F0F0F0F0_0F0F0F0F0F0F0F0F;
    matrix[1] = 128'h00000000000000AB_C000000000000000;

    whole_element = matrix[1];
    element_part = matrix[1][71:60];
    element_bit_set = matrix[0][0];
    element_bit_clear = matrix[0][4];

    idx = 1;
    element_by_variable = matrix[idx];
    element_part_by_variable = matrix[idx][71:60];
    idx = 0;
    element_by_variable_at_zero = matrix[idx];
  end

  final begin
    if (whole_element !== 128'h00000000000000AB_C000000000000000)
      $fatal(1, "matrix[1] was %h, expected 00000000000000abc000000000000000",
             whole_element);
    if (element_by_variable !== 128'h00000000000000AB_C000000000000000)
      $fatal(1, "matrix[idx] was %h, expected the same element",
             element_by_variable);
    if (element_by_variable_at_zero !==
        128'hF0F0F0F0F0F0F0F0_0F0F0F0F0F0F0F0F)
      $fatal(1, "matrix[idx] at 0 was %h, expected the other element",
             element_by_variable_at_zero);
    if (element_part !== 12'hABC)
      $fatal(1, "matrix[1][71:60] was %h, expected abc", element_part);
    if (element_part_by_variable !== 12'hABC)
      $fatal(1, "matrix[idx][71:60] was %h, expected abc",
             element_part_by_variable);
    if (element_bit_set !== 1'b1)
      $fatal(1, "matrix[0][0] was %b, expected 1", element_bit_set);
    if (element_bit_clear !== 1'b0)
      $fatal(1, "matrix[0][4] was %b, expected 0", element_bit_clear);
    $display("All checks passed");
  end
endmodule
