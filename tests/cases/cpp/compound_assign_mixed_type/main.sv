module Top;
  int          mixed_and_result;
  int          mixed_or_result;
  logic [15:0] narrowed_add;

  initial begin
    logic [3:0] y;
    logic [7:0] w;
    int         x;

    // 4-state operand widens to int via slang's per-operand Conversion.
    // Frontend rebuilds rhs typed to lhs.type so runtime stays at one shape.
    x = 32'hFFFF_FF0F;
    y = 4'b1010;
    x &= y;
    mixed_and_result = x;

    x = 32'h0000_0000;
    w = 8'b0000_0011;
    x |= w;
    mixed_or_result = x;

    // Wider literal on the rhs side; assignment narrows back to slice width
    // via the inserted ConversionExpr at the frontend.
    narrowed_add = 16'h0010;
    narrowed_add[7:0] += 16'h0123;
  end
endmodule
