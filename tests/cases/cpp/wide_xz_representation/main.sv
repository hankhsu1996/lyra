module Top;
  logic [67:0] w;
  logic [7:0] xz_lo_rt;
  logic [7:0] xz_hi_rt;

  initial begin
    w = 68'h0;
    w[2] = 1'bx;
    w[3] = 1'bz;
    w[64] = 1'bx;
    w[65] = 1'bz;
    xz_lo_rt = w[7:0];
    xz_hi_rt = w[67:60];
  end
endmodule
