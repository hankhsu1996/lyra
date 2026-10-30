// The await() task suspends the calling process until another process
// terminates, whether it ends normally or is forcibly terminated, and status()
// afterwards tells the two apart: FINISHED for a process that ran to the end
// of its body, KILLED for one that kill() ended (LRM 9.7). The time at which
// the caller resumes is what shows it waited rather than returned at once, and
// everything the awaited process did before terminating is in place by then.
module Top;
  int normal_wake_time, normal_finished, normal_marker_seen;
  int forced_wake_time, forced_killed;

  process quick;
  process victim;
  int quick_marker;

  initial begin
    fork
      begin
        quick = process::self();
        #10;
        quick_marker = 7;
      end
      begin
        victim = process::self();
        #100;
      end
    join_none

    #1;
    quick.await();
    normal_wake_time = $time;
    normal_marker_seen = quick_marker;
    normal_finished = (quick.status() == process::FINISHED);
  end

  initial begin
    #2;
    victim.await();
    forced_wake_time = $time;
    forced_killed = (victim.status() == process::KILLED);
  end

  initial begin
    #30;
    victim.kill();
  end

  final begin
    if (normal_wake_time !== 10)
      $fatal(1, "normal_wake_time was %0d, expected 10", normal_wake_time);
    if (normal_marker_seen !== 7)
      $fatal(1, "normal_marker_seen was %0d, expected 7", normal_marker_seen);
    if (normal_finished !== 1)
      $fatal(1, "normal_finished was %0d, expected 1", normal_finished);
    if (forced_wake_time !== 30)
      $fatal(1, "forced_wake_time was %0d, expected 30", forced_wake_time);
    if (forced_killed !== 1)
      $fatal(1, "forced_killed was %0d, expected 1", forced_killed);
    $display("All checks passed");
  end
endmodule
