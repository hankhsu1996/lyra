module Top;
  bit a, b;
  int marker;

  initial begin
    a = 0;
    b = 0;
    marker = 0;
    // Compound condition exercises the walker's BinaryExpr recursion: both
    // `a` and `b` are picked up as reads, so a transition on either kicks
    // re-evaluation of `a && b`.
    wait (a && b) marker = 1;
  end

  initial begin
    #5;
    a = 1;
    #5;
    b = 1;
  end
endmodule
