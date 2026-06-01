module Top;
  int a, b, c, d;
  initial begin
    a = 5;
    b = 10;
    c = ++a + b--;
    d = --a + ++b;
  end
endmodule
