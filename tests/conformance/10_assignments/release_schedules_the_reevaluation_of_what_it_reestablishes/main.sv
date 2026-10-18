// LRM 10.6.2: releasing a variable a continuous assignment drives
// reestablishes that assignment and schedules a reevaluation in its region, so
// the variable does not change at the release: the same process reads the
// forced value right after it, and the driver's value once the reevaluation has
// run. A force made before that reevaluation runs is in effect when it does, and
// is not undone by it. A variable nothing drives keeps the forced value until
// the next procedural assignment.
module Top;
  logic [7:0] d = 8'h01;
  logic [7:0] v;
  logic [7:0] w = 8'h00;
  assign v = d;

  initial begin
    #1;
    force v = 8'hff;
    force w = 8'hff;
    #1;
    d = 8'h02;
    #1;
    release v;
    if (v !== 8'hff) $fatal(1, "v read %h right after its release, expected ff", v);
    #0;
    if (v !== 8'h02)
      $fatal(1, "v read %h once reevaluated, expected the driver's 02", v);

    force v = 8'hff;
    #1;
    release v;
    force v = 8'h99;
    #0;
    if (v !== 8'h99)
      $fatal(1, "a pending reevaluation undid a later force: %h", v);
    release v;
    #1;
    if (v !== 8'h02) $fatal(1, "the last release showed %h, expected 02", v);

    release w;
    if (w !== 8'hff) $fatal(1, "w read %h after its release, expected ff", w);
    w = 8'h05;
    if (w !== 8'h05) $fatal(1, "w read %h after a write, expected 05", w);

    $display("All checks passed");
  end
endmodule
