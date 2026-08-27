// casex treats both an x and a z value in any bit of either the case expression
// or a case item as a do-not-care, and that bit position is left out of the
// comparison (LRM 12.5.1). Treating x that way is what separates casex from
// casez, which leaves an x to be compared exactly.
module Top;
  logic [3:0] code;
  int x_on_item;
  int x_on_expression;
  int z_on_expression;
  int unknowns_on_both;

  initial begin
    code = 4'b1010;
    x_on_item = 0;
    casex (code)
      4'b0xxx: x_on_item = 1;
      4'b10xx: x_on_item = 2;
      default: x_on_item = 99;
    endcase

    code = 4'b00xx;
    x_on_expression = 0;
    casex (code)
      4'b0000: x_on_expression = 1;
      default: x_on_expression = 99;
    endcase

    code = 4'b01zz;
    z_on_expression = 0;
    casex (code)
      4'b0100: z_on_expression = 1;
      default: z_on_expression = 99;
    endcase

    code = 4'b01xz;
    unknowns_on_both = 0;
    casex (code)
      4'b1x11: unknowns_on_both = 1;
      4'b0x11: unknowns_on_both = 2;
      default: unknowns_on_both = 99;
    endcase
  end

  final begin
    if (x_on_item !== 2)
      $fatal(1, "x_on_item was %0d, expected 2", x_on_item);
    if (x_on_expression !== 1)
      $fatal(1, "x_on_expression was %0d, expected 1", x_on_expression);
    if (z_on_expression !== 1)
      $fatal(1, "z_on_expression was %0d, expected 1", z_on_expression);
    if (unknowns_on_both !== 2)
      $fatal(1, "unknowns_on_both was %0d, expected 2", unknowns_on_both);
    $display("All checks passed");
  end
endmodule
