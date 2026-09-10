// LRM 23.9 puts a task and a function on the hierarchical path beside a named
// block, and LRM 23.6 excludes only what an *automatic* subroutine declares --
// so a static-lifetime local of a static task is named `instance.task.local`
// exactly as a named block's is named `instance.block.local`. What decides
// which scope a name reaches through is where the source declared it: a
// declaration at the task's own level belongs to the task, and one inside a
// `begin ... end` the source wrote belongs to that block, which is then named
// below the task. Neither direction of reach changes that -- the leaf is the
// same declaration whether the name climbs to it, descends to it, or crosses
// to it inside the reader's own unit.
module Child;
  task Ticker();
    int counted = 66;
    begin : inner
      int deeper = 77;
      #10;
    end
  endtask

  initial Ticker();

  initial begin : marked
    int kept = 55;
    #10;
  end

  int saw_upward_task_static = 0;

  initial begin
    #1;
    saw_upward_task_static = Top.Watcher.seen;
  end
endmodule

module Top;
  Child c ();

  task Watcher();
    int seen = 88;
    #10;
  endtask

  initial Watcher();

  if (1) begin : sib
    task Counter();
      int tallied = 99;
      #10;
    endtask
    initial Counter();
  end

  int saw_task_static = 0;
  int saw_task_block_static = 0;
  int saw_block_static = 0;
  int saw_sibling_task_static = 0;

  initial begin
    #1;
    saw_task_static = c.Ticker.counted;
    saw_task_block_static = c.Ticker.inner.deeper;
    saw_block_static = c.marked.kept;
    saw_sibling_task_static = sib.Counter.tallied;
  end

  final begin
    if (saw_task_static !== 66)
      $fatal(1, "a task's static read %0d, expected 66", saw_task_static);
    if (saw_task_block_static !== 77)
      $fatal(1, "a block inside a task read %0d, expected 77", saw_task_block_static);
    if (saw_block_static !== 55)
      $fatal(1, "a named block's static read %0d, expected 55", saw_block_static);
    if (c.saw_upward_task_static !== 88)
      $fatal(1, "a task's static read upward was %0d, expected 88", c.saw_upward_task_static);
    if (saw_sibling_task_static !== 99)
      $fatal(
          1, "a task's static in a sibling scope read %0d, expected 99",
          saw_sibling_task_static);
    $display("All checks passed");
  end
endmodule
