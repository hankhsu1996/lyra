module Top;
  int a, b, sum;
  int result_at_t1, result_after_a_change, result_after_b_change;

  initial begin
    a = 1;
    b = 2;
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
  end

  always @* sum = a + b;
endmodule
