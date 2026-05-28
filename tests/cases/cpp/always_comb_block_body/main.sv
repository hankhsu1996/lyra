module Top;
  int a, b, sum, prod;

  initial begin
    a = 4;
    b = 6;
  end

  always_comb begin
    sum = a + b;
    prod = a * b;
  end
endmodule
