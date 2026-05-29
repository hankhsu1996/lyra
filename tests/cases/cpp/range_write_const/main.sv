module Top;
  bit [15:0] data;
  bit [15:0] nibble_high;
  bit [15:0] byte_low;
  initial begin
    data = 16'hABCD;
    nibble_high = 16'hABCD;
    nibble_high[15:12] = 4'hF;
    byte_low = 16'hABCD;
    byte_low[7:0] = 8'hEF;
  end
endmodule
