// casez treats a z value, which a literal may also write as ?, in any bit of
// either the case expression or a case item as a do-not-care, and that bit
// position is left out of the comparison (LRM 12.5.1). x is not a do-not-care
// for casez, so a bit holding x on either side is still compared exactly and
// fails against a bit holding 0 or 1 (LRM 12.5).
module Top;
  logic [3:0] code;
  int z_on_item;
  int z_on_expression;
  int z_on_both;
  int x_on_expression;
  int x_on_item;

  initial begin
    code = 4'b1010;
    z_on_item = 0;
    casez (code)
      4'b0???: z_on_item = 1;
      4'b10??: z_on_item = 2;
      default: z_on_item = 99;
    endcase

    code = 4'b00zz;
    z_on_expression = 0;
    casez (code)
      4'b0000: z_on_expression = 1;
      default: z_on_expression = 99;
    endcase

    code = 4'b01zz;
    z_on_both = 0;
    casez (code)
      4'b1?11: z_on_both = 1;
      4'b0?11: z_on_both = 2;
      default: z_on_both = 99;
    endcase

    code = 4'b010x;
    x_on_expression = 0;
    casez (code)
      4'b0100: x_on_expression = 1;
      default: x_on_expression = 99;
    endcase

    code = 4'b0100;
    x_on_item = 0;
    casez (code)
      4'b010x: x_on_item = 1;
      default: x_on_item = 99;
    endcase
  end

  final begin
    if (z_on_item !== 2)
      $fatal(1, "z_on_item was %0d, expected 2", z_on_item);
    if (z_on_expression !== 1)
      $fatal(1, "z_on_expression was %0d, expected 1", z_on_expression);
    if (z_on_both !== 2)
      $fatal(1, "z_on_both was %0d, expected 2", z_on_both);
    if (x_on_expression !== 99)
      $fatal(1, "x_on_expression was %0d, expected 99", x_on_expression);
    if (x_on_item !== 99)
      $fatal(1, "x_on_item was %0d, expected 99", x_on_item);
    $display("All checks passed");
  end
endmodule
