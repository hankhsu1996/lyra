module Top;
  byte a;
  byte b;
  bit [15:0] raw_bits;

  initial begin
    a = -1;
    b = -2;
    raw_bits = {a, b};
  end
endmodule
