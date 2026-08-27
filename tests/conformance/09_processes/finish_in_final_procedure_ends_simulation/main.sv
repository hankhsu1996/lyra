// Execution of $finish from within a final procedure ends the simulation
// immediately, so the statements standing after it in that procedure never run
// (LRM 9.2.3) -- and if one ever did, it would end the simulation with a
// failure status.
module Top;
  int reached;

  initial #5 reached = 1;

  final begin
    if (reached !== 1)
      $fatal(1, "reached was %0d, expected 1", reached);
    $display("All checks passed");
    $finish;
    $fatal(1, "a statement after $finish in a final procedure executed");
  end
endmodule
