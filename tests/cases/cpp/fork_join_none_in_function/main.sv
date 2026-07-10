`timescale 1ns / 1ns
module Test;
  // LRM 13.4.4: a function cannot suspend, so only `join_none` is legal here --
  // it spawns without awaiting. The branch borrows `local_v`, an automatic of
  // the function's frame, and reads it after that frame has returned (LRM 6.21).
  function automatic int spawn_branches(int base);
    automatic int local_v = base + 1;
    fork
      #10 $display("[%0d] branch reads local_v=%0d", $time, local_v);
    join_none
    return base;
  endfunction

  initial begin
    $display("[%0d] function returned %0d", $time, spawn_branches(7));
    #20;
  end
endmodule
