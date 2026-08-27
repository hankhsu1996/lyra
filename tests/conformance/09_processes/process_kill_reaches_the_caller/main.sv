// kill() terminates the given process together with all its descendant
// subprocesses (LRM 9.7), so a process that kills itself, and a process that
// kills one of its own ancestors, are both among the processes terminated: the
// statement after the call does not run. A handle held from outside the killed
// subtree reports KILLED for the target once it has settled.
module Top;
  int self_before, self_after, self_killed;
  int inner_before, inner_after;
  int ancestor_after, ancestor_killed;

  process self_victim;
  process ancestor;

  initial begin
    fork
      begin
        self_victim = process::self();
        self_before = 1;
        self_victim.kill();
        self_after = 1;
      end
    join_none
    #1;
    self_killed = (self_victim.status() == process::KILLED);
  end

  initial begin
    fork
      begin
        ancestor = process::self();
        fork
          begin
            inner_before = 1;
            ancestor.kill();
            inner_after = 1;
          end
        join_none
        #50;
        ancestor_after = 1;
      end
    join_none
    #100;
    ancestor_killed = (ancestor.status() == process::KILLED);
  end

  final begin
    if (self_before !== 1)
      $fatal(1, "self_before was %0d, expected 1", self_before);
    if (self_after !== 0)
      $fatal(1, "self_after was %0d, expected 0", self_after);
    if (self_killed !== 1)
      $fatal(1, "self_killed was %0d, expected 1", self_killed);
    if (inner_before !== 1)
      $fatal(1, "inner_before was %0d, expected 1", inner_before);
    if (inner_after !== 0)
      $fatal(1, "inner_after was %0d, expected 0", inner_after);
    if (ancestor_after !== 0)
      $fatal(1, "ancestor_after was %0d, expected 0", ancestor_after);
    if (ancestor_killed !== 1)
      $fatal(1, "ancestor_killed was %0d, expected 1", ancestor_killed);
    $display("All checks passed");
  end
endmodule
