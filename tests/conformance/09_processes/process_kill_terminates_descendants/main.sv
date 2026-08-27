// process::kill() forcibly terminates the given process and all its
// descendant subprocesses -- the processes spawned by fork statements from the
// process being killed or from its descendants -- and status() reports KILLED
// for a process ended that way, WAITING for one still parked in a blocking
// statement (LRM 9.7). A process killed while parked never reaches the
// statements after the point it was blocked at, and neither does a descendant
// the killed process had left running.
module Top;
  int target_waiting_before, target_killed, kill_time;
  int target_after, descendant_after;

  process target;

  initial begin
    fork
      begin
        target = process::self();
        fork
          begin
            #100;
            descendant_after = 1;
          end
        join_none
        #100;
        target_after = 1;
      end
    join_none

    #1;
    target_waiting_before = (target.status() == process::WAITING);
    target.kill();
    kill_time = $time;
    target_killed = (target.status() == process::KILLED);
    #200;
  end

  final begin
    if (target_waiting_before !== 1)
      $fatal(1, "target_waiting_before was %0d, expected 1",
             target_waiting_before);
    if (kill_time !== 1)
      $fatal(1, "kill_time was %0d, expected 1", kill_time);
    if (target_killed !== 1)
      $fatal(1, "target_killed was %0d, expected 1", target_killed);
    if (target_after !== 0)
      $fatal(1, "target_after was %0d, expected 0", target_after);
    if (descendant_after !== 0)
      $fatal(1, "descendant_after was %0d, expected 0", descendant_after);
    $display("All checks passed");
  end
endmodule
