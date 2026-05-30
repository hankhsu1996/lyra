module Top;
  longint a;
  longint b;
  bit [127:0] wide128;
  bit [95:0] wide96;
  longint w128_lo;
  longint w128_hi;
  longint w96_lo;
  int w96_hi;

  initial begin
    a = 64'hFEDCBA9876543210;
    b = 64'h0123456789ABCDEF;
    wide128 = {a, b};
    w128_lo = wide128[63:0];
    w128_hi = wide128[127:64];
    wide96 = {32'h11111111, 32'h22222222, 32'h33333333};
    w96_lo = wide96[63:0];
    w96_hi = wide96[95:64];
  end
endmodule
