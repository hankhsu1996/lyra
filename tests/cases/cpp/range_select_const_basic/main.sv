module Top;
  bit [3:0] low_nibble;
  bit [3:0] high_nibble;
  bit [7:0] low_byte;
  bit [7:0] high_byte;
  initial begin
    bit [15:0] data;
    data = 16'hABCD;
    low_nibble = data[3:0];
    high_nibble = data[15:12];
    low_byte = data[7:0];
    high_byte = data[15:8];
  end
endmodule
