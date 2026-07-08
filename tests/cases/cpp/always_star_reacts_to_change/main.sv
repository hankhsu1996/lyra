module Top;
  int a, b, sum;
  int result_at_t1, result_after_a_change, result_after_b_change;
  int result_after_xscope_change;

  // Declared before the `always @*` that reads it, so its cross-unit reference
  // is only resolved when the body lowers; the sensitivity must still subscribe
  // to it.
  if (1) begin : g
    int v;
  end

  initial begin
    a = 1;
    b = 2;
    g.v = 0;
    #1;
    // result_at_t1 captures sum BEFORE the @* body has run -- LRM 9.4.2.2
    // says @* never runs at time zero, only on subsequent changes.
    result_at_t1 = sum;
    a = 5;
    #1;
    result_after_a_change = sum;
    b = 10;
    #1;
    result_after_b_change = sum;
    g.v = 20;
    #1;
    result_after_xscope_change = sum;
  end

  always @* sum = a + b + g.v;
endmodule
