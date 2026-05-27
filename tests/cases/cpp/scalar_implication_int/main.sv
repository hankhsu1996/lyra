module Top;
  int zero_implies_zero;
  int zero_implies_nonzero;
  int nonzero_implies_zero;
  int nonzero_implies_nonzero;
  initial begin
    int a;
    int b;
    a = 0; b = 0;
    zero_implies_zero = a -> b;
    a = 0; b = 5;
    zero_implies_nonzero = a -> b;
    a = 5; b = 0;
    nonzero_implies_zero = a -> b;
    a = 5; b = 7;
    nonzero_implies_nonzero = a -> b;
  end
endmodule
