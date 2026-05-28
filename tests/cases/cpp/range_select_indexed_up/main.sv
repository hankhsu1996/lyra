module Top;
  bit [3:0] slice_lo;
  bit [3:0] slice_mid;
  bit [3:0] slice_hi;
  initial begin
    bit [15:0] data;
    int idx;
    data = 16'hABCD;
    idx = 0;  slice_lo  = data[idx +: 4];
    idx = 4;  slice_mid = data[idx +: 4];
    idx = 12; slice_hi  = data[idx +: 4];
  end
endmodule
