// A fork-join_none block leaves the processes it spawns running while the
// parent continues (LRM 9.3.2, Table 9-1), and those processes keep running
// past the end of the scope that spawned them. The variables such a process
// refers to are still the enclosing scope's own: an automatic of a fork branch
// that has already terminated, and an automatic of a function frame that has
// already returned, each read back the value it held (LRM 6.21).
module Top;
  int from_terminated_branch;
  int from_returned_frame;
  int function_result;

  function automatic int spawn_reader(int base);
    automatic int frame_local = base + 1;
    fork
      #10 from_returned_frame = frame_local;
    join_none
    return base;
  endfunction

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
    $display("All checks passed");
  end
endmodule
