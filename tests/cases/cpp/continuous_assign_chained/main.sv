module Top;
  int a, b, c;

  initial a = 5;

  // Two continuous assignments form a combinational chain: a -> b -> c.
  // LRM 10.3.2 reads `b` on the RHS of `c`, so updating `a` propagates
  // through both assigns within the same time step.
  assign b = a;
  assign c = b + 1;
endmodule
