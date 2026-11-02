// A block's own variable refers to an object only until the block ends (LRM
// 6.21), and a process killed where it stands leaves every block it is in (LRM
// 9.7): each object held in a scope open at the stop is reclaimed, in a loop's
// body, in a branch, in a nested block and in a task the process had called,
// and one held in a block already left is reclaimed the same.
class C;
endclass

module Top;
  typedef std::weak_reference #(C) c_ref;

  c_ref in_finished_block;
  c_ref in_outer;
  c_ref in_loop;
  c_ref in_branch;
  c_ref in_nested;
  c_ref in_task;
  event never;
  process stopped;
  bit reached;
  bit reclaimed;

  task automatic held_by_a_task();
    automatic C c = new;
    in_task = new(c);
    reached = 1;
    @never;
  endtask

  task automatic run();
    automatic C outer = new;
    in_outer = new(outer);
    begin
      automatic C finished = new;
      in_finished_block = new(finished);
    end
    for (int i = 0; i < 2; i = i + 1) begin
      automatic C each = new;
      in_loop = new(each);
      if (i == 1) begin
        automatic C taken = new;
        in_branch = new(taken);
        begin
          automatic C nested = new;
          in_nested = new(nested);
          held_by_a_task();
        end
      end
    end
  endtask

  initial begin
    fork
      begin
        stopped = process::self();
        run();
      end
    join_none
    wait (reached);
    #1 stopped.kill();
    wait (in_finished_block.get() == null);
    wait (in_outer.get() == null);
    wait (in_loop.get() == null);
    wait (in_branch.get() == null);
    wait (in_nested.get() == null);
    wait (in_task.get() == null);
    reclaimed = 1;
  end

  final begin
    if (!reached) $fatal(1, "the process never reached its stop");
    if (!reclaimed) $fatal(1, "an object outlived the process that held it");
    $display("All checks passed");
  end
endmodule
