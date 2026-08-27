// A task's formal arguments carry values across the enable by copying. An
// input or inout formal receives the value of its actual when the task is
// enabled, and an output or inout formal is copied back to its actual when
// the task completes; an input formal is a copy, so writing it cannot reach
// the caller. Because a task may consume simulation time, the moment of the
// copy back is observable: while the task is suspended the actual still holds
// the value it had at the enable, however long ago the body wrote the formal.
// A ref formal is not copied at all, so a write through it is in the caller's
// variable at once (LRM 13.3, 13.5, 13.5.2).
module Top;
  int given;
  int carried;
  int produced;
  int echoed;

  int slow_out;
  int slow_io;
  int slow_ref;
  int mid_out;
  int mid_io;
  int mid_ref;

  task automatic my_task(input int a, input int b, inout int c,
                         output int d, output int e);
    c = c + a;
    d = b;
    e = c;
    a = 0;
  endtask

  task automatic slow(output int o, inout int io, ref int r);
    o = 42;
    io = io + 1;
    r = r + 1;
    #5;
  endtask

  initial begin
    given = 7;
    carried = 5;
    produced = 0;
    echoed = 0;
    my_task(given, 9, carried, produced, echoed);
  end

  initial begin
    slow_out = 3;
    slow_io = 100;
    slow_ref = 200;
    slow(slow_out, slow_io, slow_ref);
  end

  initial begin
    #2;
    mid_out = slow_out;
    mid_io = slow_io;
    mid_ref = slow_ref;
  end

  final begin
    if (given !== 7) $fatal(1, "given was %0d, expected 7", given);
    if (carried !== 12) $fatal(1, "carried was %0d, expected 12", carried);
    if (produced !== 9) $fatal(1, "produced was %0d, expected 9", produced);
    if (echoed !== 12) $fatal(1, "echoed was %0d, expected 12", echoed);
    if (mid_out !== 3) $fatal(1, "mid_out was %0d, expected 3", mid_out);
    if (mid_io !== 100) $fatal(1, "mid_io was %0d, expected 100", mid_io);
    if (mid_ref !== 201) $fatal(1, "mid_ref was %0d, expected 201", mid_ref);
    if (slow_out !== 42) $fatal(1, "slow_out was %0d, expected 42", slow_out);
    if (slow_io !== 101) $fatal(1, "slow_io was %0d, expected 101", slow_io);
    if (slow_ref !== 201)
      $fatal(1, "slow_ref was %0d, expected 201", slow_ref);
    $display("All checks passed");
  end
endmodule
