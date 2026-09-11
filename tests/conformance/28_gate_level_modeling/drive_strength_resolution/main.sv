// Where drivers of unequal strength meet on a net the stronger one dominates
// and determines the result; only among drivers of equal strength does the net
// type's own resolution decide, which is why a wired-or net answers
// differently for the same two values depending on how strongly each is driven
// (LRM 28.12.1, 28.12.4). A continuous assignment drives at strong strength
// unless it names another, and a strength written on a net declaration belongs
// to the driver that declaration's own assignment creates (LRM 10.3.1).
module Top;
  wire (weak0, weak1) declared_weak = 1'b0;
  assign declared_weak = 1'b1;

  wire assigned_weak;
  assign assigned_weak = 1'b1;
  assign (weak0, weak1) assigned_weak = 1'b0;

  wire pull_over_weak;
  assign (pull0, pull1) pull_over_weak = 1'b0;
  assign (weak0, weak1) pull_over_weak = 1'b1;

  wire equal_strength;
  assign (weak0, weak1) equal_strength = 1'b0;
  assign (weak0, weak1) equal_strength = 1'b1;

  wor wired_or_unequal;
  assign wired_or_unequal = 1'b0;
  assign (weak0, weak1) wired_or_unequal = 1'b1;

  wor wired_or_equal;
  assign wired_or_equal = 1'b0;
  assign wired_or_equal = 1'b1;

  logic seen_declared_weak;
  logic seen_assigned_weak;
  logic seen_pull_over_weak;
  logic seen_equal_strength;
  logic seen_wired_or_unequal;
  logic seen_wired_or_equal;

  initial begin
    #1;
    seen_declared_weak = declared_weak;
    seen_assigned_weak = assigned_weak;
    seen_pull_over_weak = pull_over_weak;
    seen_equal_strength = equal_strength;
    seen_wired_or_unequal = wired_or_unequal;
    seen_wired_or_equal = wired_or_equal;
  end

  final begin
    if (seen_declared_weak !== 1'b1)
      $fatal(1, "seen_declared_weak was %b, expected 1", seen_declared_weak);
    if (seen_assigned_weak !== 1'b1)
      $fatal(1, "seen_assigned_weak was %b, expected 1", seen_assigned_weak);
    if (seen_pull_over_weak !== 1'b0)
      $fatal(1, "seen_pull_over_weak was %b, expected 0", seen_pull_over_weak);
    if (seen_equal_strength !== 1'bx)
      $fatal(1, "seen_equal_strength was %b, expected x", seen_equal_strength);
    if (seen_wired_or_unequal !== 1'b0)
      $fatal(1, "seen_wired_or_unequal was %b, expected 0",
             seen_wired_or_unequal);
    if (seen_wired_or_equal !== 1'b1)
      $fatal(1, "seen_wired_or_equal was %b, expected 1", seen_wired_or_equal);
    $display("All checks passed");
  end
endmodule
