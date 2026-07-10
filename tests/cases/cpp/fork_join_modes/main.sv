`timescale 1ns / 1ns
module Test;
  // LRM 9.3.2 Table 9-1: the three join modes differ only in when the forking
  // process resumes -- after every branch, after the first, or immediately.
  // Branches carry their own delay so each resume point is observable, and a
  // branch outliving its fork keeps running.
  initial begin
    fork
      #10 $display("[%0d] join A", $time);
      #20 $display("[%0d] join B", $time);
    join
    $display("[%0d] after join", $time);

    fork
      #10 $display("[%0d] join_any A", $time);
      #30 $display("[%0d] join_any B", $time);
    join_any
    $display("[%0d] after join_any", $time);

    fork
      #5 $display("[%0d] join_none A", $time);
    join_none
    $display("[%0d] after join_none", $time);
  end
endmodule
