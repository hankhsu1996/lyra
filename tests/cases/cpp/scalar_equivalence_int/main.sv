module Top;
  int zero_eq_zero;
  int zero_eq_nonzero;
  int nonzero_eq_zero;
  int nonzero_eq_nonzero;
  initial begin
    int a;
    int b;
    a = 0; b = 0;
    zero_eq_zero = a <-> b;
    a = 0; b = 5;
    zero_eq_nonzero = a <-> b;
    a = 5; b = 0;
    nonzero_eq_zero = a <-> b;
    a = 5; b = 7;
    nonzero_eq_nonzero = a <-> b;
  end
endmodule
