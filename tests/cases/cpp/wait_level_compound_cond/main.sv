module Top;
  bit a, b;
  int marker;

  initial begin
    a = 0;
    b = 0;
    marker = 0;
    // Compound `a && b` cond: both operands enter the read set, so a
    // transition on either kicks re-evaluation.
    wait (a && b) marker = 1;
  end

  initial begin
    #5;
    a = 1;
    #5;
    b = 1;
  end
endmodule
