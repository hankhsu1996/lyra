module Top;
  bit [0:0] one_bit_high;
  bit [0:0] one_bit_low;
  initial begin
    bit [7:0] data;
    data = 8'b0000_1000;
    one_bit_high = data[3:3];
    one_bit_low  = data[2:2];
  end
endmodule
