// status() reports how a process ended: FINISHED when it ran to the end of its
// body, and KILLED when it was forcibly terminated, by kill() or by a disable
// (LRM 9.7). Both forms of disable are forceful -- disable fork ends the
// descendants of the calling process (LRM 9.6.3), and disabling a named block
// ends the activity enabled within it (LRM 9.6.2) -- and a handle taken before
// the process ended still answers for it afterwards. A process that had
// already terminated normally is not reclassified by a later disable.
module Top;
  int fork_killed, block_killed, finished_seen;

  process fork_child;
  process block_child;
  process normal_child;

  initial begin
    fork
      begin
        fork_child = process::self();
        #100;
      end
    join_none
    #1;
    disable fork;
    fork_killed = (fork_child.status() == process::KILLED);
  end

  initial begin : holder
    fork
      begin
        block_child = process::self();
        #100;
      end
    join_none
    fork
      begin
        normal_child = process::self();
        #5;
      end
    join_none
    #100;
  end

  initial begin
    #10;
    disable Top.holder;
    #20;
    block_killed = (block_child.status() == process::KILLED);
    finished_seen = (normal_child.status() == process::FINISHED);
  end

  final begin
    if (fork_killed !== 1)
      $fatal(1, "fork_killed was %0d, expected 1", fork_killed);
    if (block_killed !== 1)
      $fatal(1, "block_killed was %0d, expected 1", block_killed);
    if (finished_seen !== 1)
      $fatal(1, "finished_seen was %0d, expected 1", finished_seen);
    $display("All checks passed");
  end
endmodule
