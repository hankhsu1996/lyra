module Top;
  int a, b, c;

  initial begin
    a = 1;
    b = 2;
  end

  always_comb c = a + b;
endmodule
