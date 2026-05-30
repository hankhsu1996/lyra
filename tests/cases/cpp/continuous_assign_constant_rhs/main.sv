module Top;
  int x;

  // Empty RHS read set: the assignment fires once at t=0 (the always_comb-
  // style entry evaluation per LRM 9.2.2.2.1) and then suspends forever,
  // since SensitivityWait on an empty set never wakes up.
  assign x = 42;
endmodule
