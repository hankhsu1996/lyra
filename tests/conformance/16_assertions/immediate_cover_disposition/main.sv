// @reports-nothing:
//
// An immediate cover statement names the success of its expression as a
// coverage goal, which inverts the disposition an assert has: the statement it
// carries is what a true expression selects, and a false expression is not a
// failure, so it neither runs that statement nor issues any report (LRM 16.3).
// A cover therefore has no fail arm at all, and a run over a design whose
// covers are only partly reached is a run a conforming tool has no comment on.
module Top;
  int hits;
  int completed;

  initial begin
    hits = 0;
    completed = 0;

    cover (1) hits = hits + 1;

    cover (0) hits = hits + 1;

    cover (1) hits = hits + 1;

    cover (0);

    completed = 1;
  end

  final begin
    if (completed !== 1)
      $fatal(1, "a cover statement stopped the procedure that reached it");
    if (hits !== 2) $fatal(1, "hits was %0d, expected 2", hits);
    $display("All checks passed");
  end
endmodule
