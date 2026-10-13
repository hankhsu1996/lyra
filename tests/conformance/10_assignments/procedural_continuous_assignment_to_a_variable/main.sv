// LRM 10.6 procedural continuous assignments take a variable over with a
// continuously evaluated expression. An `assign` overrides every procedural
// assignment to the variable (LRM 10.6.1), and a `force` outranks an active
// `assign` as well (LRM 10.6.2); in both, a change to the right-hand side
// reevaluates it while the takeover is in effect, exactly as a continuous
// assignment is reevaluated. Releasing a variable that has an active `assign`
// underneath reestablishes that assignment, while a `deassign` leaves the
// variable at the value it was last given rather than restoring whatever a
// procedural assignment wrote while it was overridden. The variable may be of
// any singular type (LRM 6.4), so a real, a string and a class handle are taken
// over and handed back the same way.
class Box;
  int id;
  function new(int id);
    this.id = id;
  endfunction
endclass

module Top;
  logic [3:0] src = 4'd3;
  logic [3:0] v;

  real r = 1.5;
  shortreal sr = 1.5;
  string s = "start";
  Box box = new(1);
  Box other = new(2);

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

    force r = 2.5;
    force sr = 2.5;
    force s = "forced";
    force box = other;
    #1;
    r = 3.5;
    sr = 3.5;
    s = "written";
    box = null;
    if (r != 2.5 || sr != 2.5 || s != "forced" || box.id != 2)
      $fatal(1, "a write under a force reached a variable: %f %f %s %0d", r,
             sr, s, box.id);
    release r;
    release sr;
    release s;
    release box;
    #1;
    if (r != 2.5 || sr != 2.5 || s != "forced" || box.id != 2)
      $fatal(1, "a released variable lost its forced value: %f %f %s %0d", r,
             sr, s, box.id);
    r = 4.5;
    s = "after";
    box = null;
    if (r != 4.5 || s != "after" || box != null)
      $fatal(1, "a write after a release did not land: %f %s", r, s);

    assign r = sr + 1.0;
    #1;
    if (r != 3.5) $fatal(1, "an assign of a real left %f, expected 3.5", r);
    sr = 6.0;
    #1;
    if (r != 7.0)
      $fatal(1, "an assign of a real did not track its operand, left %f", r);
    deassign r;

    $display("All checks passed");
  end
endmodule
