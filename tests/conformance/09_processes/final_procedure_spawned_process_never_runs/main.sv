// A final procedure runs at the end of simulation, admits only the statements
// a function declaration admits, and executes in zero time; no remaining
// scheduled event executes once every final procedure has run (LRM 9.2.3). A
// process a final procedure spawns with fork-join_none therefore never starts:
// the procedure carries on past the join_none without it having run (LRM
// 9.3.2, Table 9-1), and nothing runs it afterwards -- so if it ever executed,
// it would end the simulation with a failure status.
module Top;
  int branch_ran;
  int initial_done;

  initial #5 initial_done = 1;

  final begin
    fork
      begin
        branch_ran = 1;
        $fatal(1, "a process spawned in a final procedure executed");
      end
    join_none
    if (branch_ran !== 0)
      $fatal(1, "branch_ran was %0d, expected 0", branch_ran);
    if (initial_done !== 1)
      $fatal(1, "initial_done was %0d, expected 1", initial_done);
    $display("All checks passed");
  end
endmodule
