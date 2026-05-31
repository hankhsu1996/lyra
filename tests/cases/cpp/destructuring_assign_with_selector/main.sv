module Top;
  byte a;
  byte b;

  initial begin
    a = 8'h05;
    b = 8'h03;
    // Distribute 8'hFC MSB-first into a[7:4] (gets 'hF) and b[7:4] (gets 'hC).
    // a low nibble stays 'h5, b low nibble stays 'h3.
    {a[7:4], b[7:4]} = 8'hFC;
  end
endmodule
