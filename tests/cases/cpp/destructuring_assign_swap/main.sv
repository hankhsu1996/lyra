module Top;
  byte a;
  byte b;

  initial begin
    a = 8'hAA;
    b = 8'hBB;
    {a, b} = {b, a};
  end
endmodule
