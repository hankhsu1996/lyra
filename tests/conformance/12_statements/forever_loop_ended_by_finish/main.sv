// A forever-loop has no control expression of its own, so what ends one may be
// a simulation control task: $finish stops the simulation on the pass that
// calls it, and a final procedure still executes afterwards, which is what
// leaves the loop's own state observable (LRM 12.7.6, 20.2, 9.2.3).
module Top;
  int counter;

  initial begin
    counter = 0;
    forever begin
      counter = counter + 1;
      if (counter >= 5) $finish;
    end
  end

  final begin
    if (counter !== 5) $fatal(1, "counter was %0d, expected 5", counter);
    $display("All checks passed");
  end
endmodule
