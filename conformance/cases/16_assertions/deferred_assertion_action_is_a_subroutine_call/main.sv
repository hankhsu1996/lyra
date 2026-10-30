// A deferred assertion's pass and fail statements are each one subroutine
// call, and the subroutine can be a task (LRM 16.4). An observed (`#0`)
// assertion schedules the call in the Reactive region, so a task called there
// may wait, and may disable a named block. An argument passed by value is
// evaluated where the assertion is, and one passed by reference reads the
// variable as it is when the call runs. A deferred cover carries a pass
// statement the same way, and a deferred assertion outside procedural code is
// one inside an always_comb procedure (LRM 16.4.3).
//
// Arguments are evaluated on every failure, even where the report is then
// flushed (LRM 16.4.1): a procedure resuming from an event control before the
// report matures withdraws it, and the call never runs (LRM 16.4.2), but what
// its argument called has already happened.
module Top;
  typedef int ints_t[$];

  int v = 0;
  int r = 0;
  int started[$];
  int resumed[$];
  int by_value[$];
  int by_ref[$];

  int passed = 0;
  int covered = 0;
  int watched = 0;
  int evaluated = 0;
  int flushed_ran = 0;

  logic s = 1;
  int static_saw = -1;

  event e;

  task automatic later(int value, ref int current);
    started.push_back($time);
    by_value.push_back(value);
    by_ref.push_back(current);
    #2 resumed.push_back($time);
  endtask

  task automatic stop_blk();
    disable blk;
  endtask

  task automatic count_pass();
    passed = passed + 1;
  endtask

  task automatic hit();
    covered = covered + 1;
  endtask

  function automatic int evaluate();
    evaluated = evaluated + 1;
    return evaluated;
  endfunction

  task automatic must_not_run(int value);
    flushed_ran = flushed_ran + 1;
  endtask

  function automatic void note_static(int value);
    static_saw = value;
  endfunction

  initial begin : blk
    #20 watched = 1;
  end

  initial begin
    #1;
    v = 1;
    r = 1;
    assert #0 (v == 0) else later(v, r);
    v = 2;
    r = 2;
    assert #0 (1) count_pass();
    cover #0 (v == 2) hit();
    #1;
    assert #0 (0) else stop_blk();
  end

  initial begin
    #5;
    assert #0 (0) else must_not_run(evaluate());
    @e;
  end

  initial begin
    #5;
    #0 -> e;
  end

  assert #0 (s) else note_static(10);

  initial #7 s = 0;

  final begin
    if (started != ints_t'{1})
      $fatal(1, "the task started at %p, expected 1", started);
    if (by_value != ints_t'{1})
      $fatal(1, "the by-value argument was %p, expected 1", by_value);
    if (by_ref != ints_t'{2})
      $fatal(1, "the ref argument read %p, expected 2", by_ref);
    if (resumed != ints_t'{3})
      $fatal(1, "the task resumed at %p, expected 3", resumed);
    if (passed !== 1) $fatal(1, "the pass call ran %0d times, expected 1",
                             passed);
    if (covered !== 1) $fatal(1, "the cover call ran %0d times, expected 1",
                              covered);
    if (watched !== 0) $fatal(1, "the disabled block went on to its end");
    if (evaluated !== 1)
      $fatal(1, "the flushed report's argument was evaluated %0d times",
             evaluated);
    if (flushed_ran !== 0) $fatal(1, "a flushed report's call ran");
    if (static_saw !== 10)
      $fatal(1, "the static assertion's call saw %0d, expected 10",
             static_saw);
    $display("All checks passed");
  end
endmodule
