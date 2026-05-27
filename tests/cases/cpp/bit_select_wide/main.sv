module Top;
  bit lo;
  bit mid_low;
  bit mid_high;
  bit hi;
  initial begin
    bit [127:0] w;
    w = 128'h8000_0000_0000_0001_0000_0000_0000_0001;
    lo = w[0];
    mid_low = w[63];
    mid_high = w[64];
    hi = w[127];
  end
endmodule
