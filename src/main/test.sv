module Test;
  int a, b, c;
  initial begin
    a = 1;
    b = 2;
  end

  always_comb begin
    c = a + b;
  end

endmodule
