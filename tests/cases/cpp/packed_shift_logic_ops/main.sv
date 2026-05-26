module Top;
  logic [7:0] shl_;
  logic [7:0] shr_logical;
  logic signed [7:0] shr_arith_pos;
  logic signed [7:0] shr_arith_neg;
  logic [3:0] shl_overflow;
  logic [3:0] shr_overflow;
  initial begin
    logic [7:0] a;
    a = 8'b10110100;
    shl_ = a << 2;
    shr_logical = a >> 2;
    begin
      logic signed [7:0] s;
      s = 8'sb00110100;
      shr_arith_pos = s >>> 2;
      s = 8'sb11001100;
      shr_arith_neg = s >>> 2;
    end
    begin
      logic [3:0] x;
      x = 4'b1010;
      shl_overflow = x << 8;
      shr_overflow = x >> 8;
    end
  end
endmodule
