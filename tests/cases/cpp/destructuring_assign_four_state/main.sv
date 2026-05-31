module Top;
  logic [7:0] a;
  logic [7:0] b;

  initial begin
    // 16-bit RHS, MSB-first split into a (high byte) and b (low byte).
    {a, b} = 16'b10xx_1100_zz11_0101;
  end
endmodule
