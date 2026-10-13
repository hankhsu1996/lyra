// A delay on a net declaration that does not assign is a net delay: every
// change a driver makes to the net takes that long to take effect (LRM 10.3.3,
// 28.16). It sits between the drivers and the net, so it applies to a change
// from a continuous assignment written separately, and a delay that assignment
// carries itself comes before it.
module Top;
  logic source;
  wire #10 delayed;
  assign delayed = source;

  wire #10 delayed_twice;
  assign #5 delayed_twice = source;

  bit finished;

  initial begin
    source = 1'b0;
    #20;
    source = 1'b1;
    #9;
    if (delayed !== 1'b0)
      $fatal(1, "delayed was %b at %0t, expected 0", delayed, $time);
    #2;
    if (delayed !== 1'b1)
      $fatal(1, "delayed was %b at %0t, expected 1", delayed, $time);
    #3;
    if (delayed_twice !== 1'b0)
      $fatal(1, "delayed_twice was %b at %0t, expected 0", delayed_twice,
             $time);
    #2;
    if (delayed_twice !== 1'b1)
      $fatal(1, "delayed_twice was %b at %0t, expected 1", delayed_twice,
             $time);
    finished = 1'b1;
  end

  final begin
    if (finished !== 1'b1) $fatal(1, "the checks did not all run");
    $display("All checks passed");
  end
endmodule
