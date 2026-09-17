// A fork block leaves processes running once control passes out of it, and
// those processes keep running past the end of the scope that spawned them:
// join_none never waits, and join_any waits only for the first to terminate
// (LRM 9.3.2, Table 9-1). The variables such a process refers to are still the
// enclosing scope's own: an automatic of a fork branch that has already
// terminated, an automatic of a function frame that has already returned, and
// an automatic of a task frame the first branch released, each read back the
// value it held (LRM 6.21).
module Top;
  int from_terminated_branch;
  int from_returned_frame;
  int from_released_frame;
  int function_result;

  function automatic int spawn_reader(int base);
    automatic int frame_local = base + 1;
    fork
      #10 from_returned_frame = frame_local;
    join_none
    return base;
  endfunction

  task automatic race_reader(int base);
    automatic int frame_local = base + 2;
    fork
      #1;
      #10 from_released_frame = frame_local;
    join_any
  endtask

  initial begin
    fork
      begin
        automatic int branch_local = 7;
        fork
          #10 from_terminated_branch = branch_local;
        join_none
      end
    join_none
    function_result = spawn_reader(7);
    race_reader(7);
    #20;
  end

  final begin
    if (function_result !== 7)
      $fatal(1, "function_result was %0d, expected 7", function_result);
    if (from_terminated_branch !== 7)
      $fatal(1, "from_terminated_branch was %0d, expected 7",
             from_terminated_branch);
    if (from_returned_frame !== 8)
      $fatal(1, "from_returned_frame was %0d, expected 8",
             from_returned_frame);
    if (from_released_frame !== 9)
      $fatal(1, "from_released_frame was %0d, expected 9",
             from_released_frame);
    $display("All checks passed");
  end
endmodule
