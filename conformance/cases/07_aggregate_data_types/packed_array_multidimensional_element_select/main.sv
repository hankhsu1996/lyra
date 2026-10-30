// A packed array is a contiguous set of bits treated as a single vector, so a
// vector of the same width may be assigned to one and read back element by
// element or as a whole. The dimensions written before the identifier are the
// packed dimensions, and the rightmost of them varies most rapidly, so the
// leftmost dimension's highest index names the most significant part of the
// vector (LRM 7.4.1, 7.4.4, 7.4.6).
module Top;
  bit [3:0][7:0] declared_init = 32'hAABBCCDD;
  bit [7:0] init_byte3;
  bit [7:0] init_byte2;
  bit [7:0] init_byte1;
  bit [7:0] init_byte0;

  bit [31:0] flat = 32'h11223344;
  bit [7:0] assigned_byte3;
  bit [7:0] assigned_byte0;
  bit [31:0] read_back;

  bit [7:0] nested_lowest;
  bit [7:0] nested_highest;
  bit [15:0] nested_row;

  initial begin
    bit [3:0][7:0] assigned;
    bit [1:0][1:0][7:0] nested;

    init_byte3 = declared_init[3];
    init_byte2 = declared_init[2];
    init_byte1 = declared_init[1];
    init_byte0 = declared_init[0];

    assigned = flat;
    assigned_byte3 = assigned[3];
    assigned_byte0 = assigned[0];
    read_back = assigned;

    nested = 32'h11223344;
    nested_lowest = nested[0][0];
    nested_highest = nested[1][1];
    nested_row = nested[1];
  end

  final begin
    if (init_byte3 !== 8'hAA)
      $fatal(1, "init_byte3 was %h, expected aa", init_byte3);
    if (init_byte2 !== 8'hBB)
      $fatal(1, "init_byte2 was %h, expected bb", init_byte2);
    if (init_byte1 !== 8'hCC)
      $fatal(1, "init_byte1 was %h, expected cc", init_byte1);
    if (init_byte0 !== 8'hDD)
      $fatal(1, "init_byte0 was %h, expected dd", init_byte0);
    if (assigned_byte3 !== 8'h11)
      $fatal(1, "assigned_byte3 was %h, expected 11", assigned_byte3);
    if (assigned_byte0 !== 8'h44)
      $fatal(1, "assigned_byte0 was %h, expected 44", assigned_byte0);
    if (read_back !== 32'h11223344)
      $fatal(1, "read_back was %h, expected 11223344", read_back);
    if (nested_lowest !== 8'h44)
      $fatal(1, "nested_lowest was %h, expected 44", nested_lowest);
    if (nested_highest !== 8'h11)
      $fatal(1, "nested_highest was %h, expected 11", nested_highest);
    if (nested_row !== 16'h1122)
      $fatal(1, "nested_row was %h, expected 1122", nested_row);
    $display("All checks passed");
  end
endmodule
