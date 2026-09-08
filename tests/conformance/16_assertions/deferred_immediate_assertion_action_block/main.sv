// @reports-nothing:
//
// A deferred immediate assertion holds its action for a later region of the
// time step rather than running it inline (LRM 16.4), and the outcome still
// selects which arm that action is. A true expression selects the pass arm, so
// a passing assertion carrying no pass statement runs nothing at all and
// reaches no default report -- an observed (`#0`) and a final one alike.
//
// An assertion has no other effect on the design: it never writes a variable
// the design can read, and the report a failing one reaches is the tool's, at a
// severity that does not end the run. So the program cannot observe the silence
// directly, which is what the directive above is for -- and what it checks
// instead is the other half of what deferral means. Holding an action for a
// later region must not hold the procedure that reached it, so the procedure
// runs to its end and says so.
module Top;
  int completed;

  initial begin
    completed = 0;
    assert #0 (1);
    assert final (1);
    completed = 1;
  end

  final begin
    if (completed !== 1)
      $fatal(1, "a passing deferred assertion stopped its procedure");
    $display("All checks passed");
  end
endmodule
