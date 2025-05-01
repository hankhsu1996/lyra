module Test;
  int a, b, c;
  initial begin
    a = 1;
    #5 b = 2;
    $finish();
  end

  always_comb begin
    c = a + b;
  end

endmodule
