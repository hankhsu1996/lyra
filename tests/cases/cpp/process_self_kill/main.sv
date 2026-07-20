`timescale 1ns / 1ns
module Test;
  // LRM 9.7 process::kill() on the calling process. Killing the running process
  // terminates it at a safe boundary: statements after the kill do not run, and
  // the process is reported KILLED. This is the self case that needs deferred,
  // safe-boundary teardown -- the running frame cannot be destroyed
  // synchronously, so termination is requested (registrations revoked, unwind
  // arranged) and published only when the body has unwound. Killing a
  // non-running sibling is covered by process_kill_await.
  int pre_kill;
  int post_kill;
  int killed_ok;
  process victim;

  initial begin
    fork
      begin
        victim = process::self();
        pre_kill = 1;
        victim.kill();
        post_kill = 1;
      end
    join_none
    #1;
    killed_ok = (victim.status() == process::KILLED);
  end
endmodule
