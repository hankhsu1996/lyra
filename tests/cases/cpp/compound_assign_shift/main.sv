module Top;
  logic [15:0] lshift_logical;
  logic [15:0] rshift_logical;
  int          lshift_arith;
  int          rshift_arith;

  initial begin
    lshift_logical = 16'h0001;
    lshift_logical <<= 4;

    rshift_logical = 16'hF000;
    rshift_logical >>= 4;

    lshift_arith = 32'sd1;
    lshift_arith <<<= 4;

    rshift_arith = -32'sd16;
    rshift_arith >>>= 2;
  end
endmodule
