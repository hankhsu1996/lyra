module Top;
  bit [3:0] slice_top;
  bit [3:0] slice_mid;
  bit [3:0] slice_lo;
  initial begin
    bit [15:0] data;
    int idx;
    data = 16'hABCD;
    idx = 15; slice_top = data[idx -: 4];
    idx = 7;  slice_mid = data[idx -: 4];
    idx = 3;  slice_lo  = data[idx -: 4];
  end
endmodule
