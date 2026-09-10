// A disable selects its target by static declaration identity (LRM 9.6.2) and
// a hierarchical name reaches a block or a task anywhere on the elaborated
// hierarchy (LRM 23.6), so a name that descends into a child instance ends that
// instance's activity and no other's. The target is replicated with the scope
// that declares it, which is what makes a sibling instance of the same module,
// and another element of the same instance array, untouched by a disable that
// named one of them. Whichever declaration a route ends at -- a named block, a
// block inside a task, or the task itself -- the reach is the same walk a read
// of a declaration there takes: through an array element, through a generate
// block, and through a nested instance alike.
module Worker;
  int reached = 0;
  int finished = 0;
  int task_reached = 0;
  int task_finished = 0;
  int after_task = 0;

  initial begin : work
    #1;
    reached = 1;
    #10;
    finished = 1;
  end

  task automatic span();
    begin : window
      #1;
      task_reached = 1;
      #10;
      task_finished = 1;
    end
    after_task = 1;
  endtask

  initial span();
endmodule

module Bank;
  Worker w[2] ();
  if (1) begin : g
    Worker inner ();
  end
endmodule

module Top;
  Bank b ();

  initial begin
    #2;
    // An instance-array element, a generate block on the path, and a block
    // declared inside a task each end at the same kind of target.
    disable b.w[0].work;
    disable b.g.inner.work;
    disable b.w[0].span.window;
    // The task itself, rather than a block inside it.
    disable b.g.inner.span;
  end

  final begin
    if (b.w[0].reached !== 1)
      $fatal(1, "w[0].reached was %0d, expected 1", b.w[0].reached);
    if (b.w[0].finished !== 0)
      $fatal(1, "w[0].finished was %0d, expected 0", b.w[0].finished);
    // The sibling element ran the same declaration undisturbed.
    if (b.w[1].finished !== 1)
      $fatal(1, "w[1].finished was %0d, expected 1", b.w[1].finished);
    if (b.g.inner.finished !== 0)
      $fatal(1, "inner.finished was %0d, expected 0", b.g.inner.finished);

    // Ending a block inside a task leaves the rest of the task running.
    if (b.w[0].task_reached !== 1)
      $fatal(1, "w[0].task_reached was %0d, expected 1", b.w[0].task_reached);
    if (b.w[0].task_finished !== 0)
      $fatal(1, "w[0].task_finished was %0d, expected 0", b.w[0].task_finished);
    if (b.w[0].after_task !== 1)
      $fatal(1, "w[0].after_task was %0d, expected 1", b.w[0].after_task);

    // Ending the task itself leaves nothing of it running, not even the
    // statement after the block it was suspended in.
    if (b.g.inner.task_reached !== 1)
      $fatal(1, "inner.task_reached was %0d, expected 1", b.g.inner.task_reached);
    if (b.g.inner.task_finished !== 0)
      $fatal(1, "inner.task_finished was %0d, expected 0",
             b.g.inner.task_finished);
    if (b.g.inner.after_task !== 0)
      $fatal(1, "inner.after_task was %0d, expected 0", b.g.inner.after_task);

    if (b.w[1].task_finished !== 1)
      $fatal(1, "w[1].task_finished was %0d, expected 1", b.w[1].task_finished);
    $display("All checks passed");
  end
endmodule
