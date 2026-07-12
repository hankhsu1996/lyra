`timescale 1ns / 1ns
module Test;
  // LRM 9.2.3: a `final` procedure admits exactly the statements a function
  // admits, so `join_none` is the one legal fork here (LRM 13.4.4), and it
  // spawns a branch of the final procedure's own process (LRM 9.5). No
  // remaining scheduled event executes once the final procedures have run, so
  // that branch never starts.
  initial begin
    #5 $display("[%0d] initial done", $time);
  end

  final begin
    $display("final enter");
    fork
      $display("final branch");
    join_none
    $display("final exit");
  end
endmodule
