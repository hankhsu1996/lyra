module Top;
  bit bit0;
  bit bit1;
  bit bit2;
  bit bit3;
  bit bit7;
  initial begin
    bit [7:0] data;
    data = 8'b0000_1101;
    bit0 = data[0];
    bit1 = data[1];
    bit2 = data[2];
    bit3 = data[3];
    bit7 = data[7];
  end
endmodule
