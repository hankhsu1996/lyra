module Top;
  int a, b, c;

  initial a = 5;

  always_comb b = a * 2;
  always_comb c = b * 3;
endmodule
