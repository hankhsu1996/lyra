module Top;
  int x;
  int y;
  int z;
  int neg;
  int compound;
  initial begin
    x = 7;
    y = 1 + 2;
    z = x + y;
    neg = -3;
    compound = neg - 4;
  end
endmodule
