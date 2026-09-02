// @reports-nothing:
//
// An immediate assert tests its expression where the statement executes and
// runs the action block arm that outcome selects: the pass statement when the
// expression is true, the fail statement when it is false, and nothing at all
// where that arm is omitted (LRM 16.3). The expression is read the way the
// condition of a procedural if is read, so x, z, and 0 are each false. Every
// assertion here either passes or supplies a fail statement of its own, and a
// conforming tool has no comment on any of them.
module Top;
  int pass_hits;
  int fail_hits;
  int unknown_fail;
  int high_z_fail;
  logic unknown;
  logic high_z;

  initial begin
    pass_hits = 0;
    fail_hits = 0;
    unknown_fail = 0;
    high_z_fail = 0;
    unknown = 1'bx;
    high_z = 1'bz;

    assert (1) pass_hits = pass_hits + 1;
    else fail_hits = fail_hits + 1;

    assert (0) pass_hits = pass_hits + 1;
    else fail_hits = fail_hits + 1;

    assert (1) pass_hits = pass_hits + 1;

    assert (1) else fail_hits = fail_hits + 1;

    assert (0) else fail_hits = fail_hits + 1;

    assert (unknown) else unknown_fail = 1;

    assert (high_z) else high_z_fail = 1;
  end

  final begin
    if (pass_hits !== 2) $fatal(1, "pass_hits was %0d, expected 2", pass_hits);
    if (fail_hits !== 2) $fatal(1, "fail_hits was %0d, expected 2", fail_hits);
    if (unknown_fail !== 1)
      $fatal(1, "an x expression left unknown_fail at %0d, expected 1", unknown_fail);
    if (high_z_fail !== 1)
      $fatal(1, "a z expression left high_z_fail at %0d, expected 1", high_z_fail);
    $display("All checks passed");
  end
endmodule
