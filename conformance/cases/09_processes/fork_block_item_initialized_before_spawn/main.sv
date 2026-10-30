// A variable declared in the block_item_declaration of a fork-join block is
// initialized whenever execution enters its scope and before any of the block's
// processes are spawned, while a variable declared inside a parallel statement
// is initialized only once that statement starts running. No process a fork
// spawns starts until the parent blocks or terminates, so the second reads what
// the parent went on to write and the first cannot (LRM 9.3.2).
module Top;
  int x = 1;
  int fork_item;
  int branch_item;

  initial begin
    fork
      automatic int k = x;
      begin
        automatic int m = x;
        fork_item = k;
        branch_item = m;
      end
    join_none
    x = 9;
    #1;
  end

  final begin
    if (fork_item !== 1)
      $fatal(1, "fork_item was %0d, expected 1", fork_item);
    if (branch_item !== 9)
      $fatal(1, "branch_item was %0d, expected 9", branch_item);
    $display("All checks passed");
  end
endmodule
