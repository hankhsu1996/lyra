module Top;
  typedef struct packed {
    logic [63:0] high;
    logic [63:0] low;
  } wide128_t;
  wide128_t w;
  longint result_high;
  longint result_low;
  initial begin
    w.high = 64'h0123456789ABCDEF;
    w.low = 64'hFEDCBA9876543210;
    result_high = w.high;
    result_low = w.low;
  end
endmodule
