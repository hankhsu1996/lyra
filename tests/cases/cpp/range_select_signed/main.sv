module Top;
  bit [3:0] low_nibble;
  bit [3:0] high_nibble;
  initial begin
    bit signed [7:0] s;
    s = 8'sb1000_0001;
    low_nibble = s[3:0];
    high_nibble = s[7:4];
  end
endmodule
