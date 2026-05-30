module Top;
  int a;
  int b;
  int eq_match;
  int eq_diff;
  int neq_match;
  int neq_diff;

  initial begin
    a = 5;
    b = 5;
    eq_match = (a === b) ? 1 : 0;
    neq_match = (a !== b) ? 1 : 0;

    b = 10;
    eq_diff = (a === b) ? 1 : 0;
    neq_diff = (a !== b) ? 1 : 0;
  end
endmodule
