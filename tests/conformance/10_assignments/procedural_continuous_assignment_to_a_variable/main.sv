// LRM 10.6 procedural continuous assignments take a variable over with a
// continuously evaluated expression. An `assign` overrides every procedural
// assignment to the variable (LRM 10.6.1), and a `force` outranks an active
// `assign` as well (LRM 10.6.2); in both, a change to the right-hand side
// reevaluates it while the takeover is in effect, exactly as a continuous
// assignment is reevaluated. Releasing a variable that has an active `assign`
// underneath reestablishes that assignment, while a `deassign` leaves the
// variable at the value it was last given rather than restoring whatever a
// procedural assignment wrote while it was overridden.
module Top;
  logic [3:0] src = 4'd3;
  logic [3:0] v;

  initial begin
    v = 4'd1;
    if (v !== 4'd1) $fatal(1, "a plain procedural write left %0d, expected 1", v);

    assign v = src;
    #1;
    if (v !== 4'd3) $fatal(1, "an assign left %0d, expected 3", v);

    v = 4'd9;
    #1;
    if (v !== 4'd3)
      $fatal(1, "a procedural write under an assign left %0d, expected 3", v);

    src = 4'd5;
    #1;
    if (v !== 4'd5) $fatal(1, "an assign did not track its operand, left %0d", v);

    force v = 4'd7;
    #1;
    if (v !== 4'd7) $fatal(1, "a force over an assign left %0d, expected 7", v);

    src = 4'd6;
    #1;
    if (v !== 4'd7) $fatal(1, "an assign outranked a force, left %0d", v);

    release v;
    #1;
    if (v !== 4'd6)
      $fatal(1, "releasing did not reestablish the assign, left %0d", v);

    deassign v;
    src = 4'd2;
    #1;
    if (v !== 4'd6) $fatal(1, "a deassigned variable still tracked, left %0d", v);

    v = 4'd4;
    if (v !== 4'd4)
      $fatal(1, "a procedural write after deassign left %0d, expected 4", v);

    $display("All checks passed");
  end
endmodule
