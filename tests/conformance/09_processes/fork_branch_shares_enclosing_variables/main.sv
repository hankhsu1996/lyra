// A parallel statement of a fork-join block names the variables of the scope
// enclosing the fork, not copies of them, and no process the fork spawns
// starts executing until the parent blocks or terminates (LRM 9.3.2). What a
// branch reads is therefore the value the parent left behind after running on
// past the fork, and what a branch writes is what the parent reads later.
// Neither depends on the lifetime of the enclosing declaration (LRM 6.21).
module Top;
  int shared_static = 1;
  int branch_saw_static;
  int branch_saw_automatic;
  int parent_saw;
  int parent_saw_time;

  initial begin
    automatic int enclosing_automatic = 7;
    fork
      begin
        branch_saw_static = shared_static;
        branch_saw_automatic = enclosing_automatic;
        #10 shared_static = 42;
      end
    join_none
    shared_static = 2;
    enclosing_automatic = 99;
    #20;
    parent_saw = shared_static;
    parent_saw_time = $time;
  end

  final begin
    if (branch_saw_static !== 2)
      $fatal(1, "branch_saw_static was %0d, expected 2", branch_saw_static);
    if (branch_saw_automatic !== 99)
      $fatal(1, "branch_saw_automatic was %0d, expected 99",
             branch_saw_automatic);
    if (parent_saw !== 42)
      $fatal(1, "parent_saw was %0d, expected 42", parent_saw);
    if (parent_saw_time !== 20)
      $fatal(1, "parent_saw_time was %0d, expected 20", parent_saw_time);
    $display("All checks passed");
  end
endmodule
