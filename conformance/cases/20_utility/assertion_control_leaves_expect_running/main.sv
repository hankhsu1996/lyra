// On, Off and Kill do not affect an expect statement, and the action controls
// do (LRM 20.11): an expect reached with checking off or killed still waits
// for its property and runs the statement its answer selects, and one reached
// with passing off goes on without running its pass statement (LRM 16.17).
module Top;
  logic clk = 0;
  logic b = 0;
  int passed = 0;

  always #5 clk = ~clk;

  initial begin
    $assertoff;
    $assertkill;
    fork
      #10 b = 1;
    join_none
    expect (@(posedge clk) ##1 b) passed = passed + 1;
    else $fatal(1, "the expect failed");
    if (passed !== 1) $fatal(1, "the expect ran %0d pass statements", passed);

    $assertpassoff;
    expect (@(posedge clk) ##1 b) passed = passed + 1;
    else $fatal(1, "the expect failed");
    if (passed !== 1) begin
      $fatal(1, "passing off, the expect ran its pass statement");
    end
    $display("All checks passed");
    $finish;
  end
endmodule
