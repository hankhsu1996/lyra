module Top;
  logic [7:0] shl_;
  logic [7:0] shr_logical;
  logic signed [7:0] shr_arith_pos;
  logic signed [7:0] shr_arith_neg;
  logic [3:0] shl_overflow;
  logic [3:0] shr_overflow;
  logic [7:0] shl_amt_xz;
  logic [7:0] shl_val_xz;
  logic [7:0] shr_val_xz;
  logic signed [7:0] ashr_val_xz_neg;
  logic signed [7:0] ashr_msb_x;
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
    begin
      logic [7:0] v;
      logic [3:0] amt;
      v = 8'b00000001;
      amt = 4'bxxxx;
      shl_amt_xz = v << amt;
      v = 8'b1011xx00;
      shl_val_xz = v << 1;
      shr_val_xz = v >> 1;
    end
    begin
      logic signed [7:0] s;
      s = 8'sb1011xx00;
      ashr_val_xz_neg = s >>> 1;
      s = 8'sbx0001110;
      ashr_msb_x = s >>> 2;
    end
  end
endmodule
