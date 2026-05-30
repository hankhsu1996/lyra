module Top;
  int a, b;

  initial a = 3;

  // LRM 10.3.2: `b` is re-evaluated whenever any operand on the RHS
  // changes; here `b` settles to `a + 1` once `a` is initialised.
  assign b = a + 1;
endmodule
