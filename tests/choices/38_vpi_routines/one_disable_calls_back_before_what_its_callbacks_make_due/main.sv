// LRM 38.36.1 orders nothing among the callbacks one occurrence makes due. A
// disable ending a block and one nested in it makes both blocks' callbacks
// due, and Lyra makes them due together before calling any, as it does what
// one write makes due, so a change the first one's routine makes is called
// back after the second: outer, inner, then the change.
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context function int place();
  import "DPI-C" context function int outer_order();
  import "DPI-C" context function int inner_order();
  import "DPI-C" context function int changed_order();

  int poked = 0;

  initial begin : outer
    begin : inner
      #100;
    end
  end

  initial begin
    if (place() !== 0) $fatal(1, "placing the callbacks failed");
    #1 disable outer;
    #1;
    if (outer_order() !== 1 || inner_order() !== 2 || changed_order() !== 3)
      $fatal(1, "outer, inner and the change were called %0d, %0d, %0d",
             outer_order(), inner_order(), changed_order());
    $display("All checks passed");
  end
endmodule
