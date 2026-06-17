module Top;
  // Positional assignment-pattern declaration initializer (LRM 7.10 / 10.9.1).
  int q [$] = '{1, 2, 3};
  // Replicated assignment-pattern declaration initializer.
  int r [$] = '{4{7}};
  // No initializer: LRM Table 6-7 default is the empty queue.
  int e [$];
endmodule
