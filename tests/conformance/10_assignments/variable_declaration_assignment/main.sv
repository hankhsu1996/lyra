// An assignment written as part of a variable declaration is a variable
// initialization rather than a continuous assignment: it places the value in
// the variable before any initial or always procedure starts, and it has no
// duration, so the variable holds that value until the next assignment to it
// (LRM 10.5). The initialized object may be an aggregate as readily as a
// scalar.
module Top;
  int scalar = 7;
  logic [7:0] vector = 8'hA5;
  int elements [3] = '{1, 2, 3};

  int scalar_at_start;
  logic [7:0] vector_at_start;
  int element_sum;
  int scalar_after_write;

  initial begin
    scalar_at_start = scalar;
    vector_at_start = vector;
    element_sum = elements[0] + elements[1] + elements[2];
    scalar = 20;
    scalar_after_write = scalar;
  end

  final begin
    if (scalar_at_start !== 7)
      $fatal(1, "scalar_at_start was %0d, expected 7", scalar_at_start);
    if (vector_at_start !== 8'hA5)
      $fatal(1, "vector_at_start was %h, expected a5", vector_at_start);
    if (element_sum !== 6)
      $fatal(1, "element_sum was %0d, expected 6", element_sum);
    if (scalar_after_write !== 20)
      $fatal(1, "scalar_after_write was %0d, expected 20",
             scalar_after_write);
    if (scalar !== 20) $fatal(1, "scalar was %0d, expected 20", scalar);
    $display("All checks passed");
  end
endmodule
