// @top: Left Right
//
// A complete path name starting at a top-level module may be used from
// anywhere in the design (LRM 23.6), including from a parallel hierarchy and
// from the hierarchy it names. So two top-level instances may each read what
// the other declares, and one may name a block of its own through the path
// from the top, which reaches the same block a name written inside it does.
module Left;
  int mine = 3;
  int seen_right = 0;

  if (1) begin : held
    int inner = 11;
  end

  initial begin
    held.inner = 13;
    #1 seen_right = Right.mine;
  end

  final begin
    if (seen_right !== 5)
      $fatal(1, "Left read Right.mine as %0d, expected 5", seen_right);
    if (Right.seen_left !== 3)
      $fatal(1, "Right read Left.mine as %0d, expected 3", Right.seen_left);
    if (Left.held.inner !== 13)
      $fatal(1, "Left.held.inner was %0d, expected 13", Left.held.inner);
    $display("All checks passed");
  end
endmodule

module Right;
  int mine = 5;
  int seen_left = 0;

  initial #1 seen_left = Left.mine;
endmodule
