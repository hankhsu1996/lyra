module Top;
  int a;
  int b;
  initial begin
    a = 0;
    b = 0;
    if (1) a = 7;
    if (0) b = 9;
  end
endmodule
