// A branch its fork does not wait for may run after the call that passed a
// `ref` argument has returned, so the argument is named only in the
// initializer of a variable the fork declares, which runs before any branch is
// spawned (LRM 9.3.2). A branch of a fork that waits for every branch ends
// before the call does, and names the argument freely.
module Top;
  int counter;
  int copied;
  int joined;

  // The fork's own variable takes the argument's value when the fork is
  // entered, so a later write to the actual does not reach what the branch
  // reads.
  task automatic copy_then_wait(ref int r);
    fork
      automatic int snapshot = r;
      #2 copied = snapshot;
    join_none
  endtask

  task automatic bump_joined(ref int r);
    fork
      #1 r = r + 1;
      #2 r = r + 10;
    join
  endtask

  initial begin
    counter = 7;
    copy_then_wait(counter);
    counter = 8;
    begin : joined_call
      automatic int local_count = 5;
      bump_joined(local_count);
      joined = local_count;
    end
  end

  final begin
    if (copied !== 7)
      $fatal(1, "a fork's copy of a ref argument read %0d, expected 7",
             copied);
    if (joined !== 16)
      $fatal(1, "a join's branches left the ref argument %0d, expected 16",
             joined);
    $display("All checks passed");
  end
endmodule
