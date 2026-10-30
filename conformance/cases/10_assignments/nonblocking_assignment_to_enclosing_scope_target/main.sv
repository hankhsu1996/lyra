// The left-hand side of a nonblocking assignment may name a variable declared
// in a scope enclosing the procedure that makes the assignment, and an element
// of an enclosing-scope array selected by the loop generate variable names one
// target per generated procedure. The update is deferred to the end of the
// time step exactly as it is for a target declared alongside the procedure, so
// a statement executed after the assignment still reads the value the target
// held before it, and only the end of the step replaces that value
// (LRM 10.4.2).
module Top;
  bit clk;
  int d;
  int captured = 9;
  int captured_per_index [2] = '{3, 4};
  int captured_before_step_end;
  int per_index_before_step_end [2];

  if (1) begin : g_if
    always_ff @(posedge clk) begin
      captured <= d;
      captured_before_step_end = captured;
    end
  end

  for (genvar i = 0; i < 2; i++) begin : g_for
    always_ff @(posedge clk) begin
      captured_per_index[i] <= d + i;
      per_index_before_step_end[i] = captured_per_index[i];
    end
  end

  initial begin
    clk = 0;
    d = 7;
    #5;
    clk = 1;
    #5;
    clk = 0;
  end

  final begin
    if (captured_before_step_end !== 9)
      $fatal(1, "captured_before_step_end was %0d, expected 9",
             captured_before_step_end);
    if (captured !== 7) $fatal(1, "captured was %0d, expected 7", captured);
    if (per_index_before_step_end[0] !== 3)
      $fatal(1, "per_index_before_step_end[0] was %0d, expected 3",
             per_index_before_step_end[0]);
    if (per_index_before_step_end[1] !== 4)
      $fatal(1, "per_index_before_step_end[1] was %0d, expected 4",
             per_index_before_step_end[1]);
    if (captured_per_index[0] !== 7)
      $fatal(1, "captured_per_index[0] was %0d, expected 7",
             captured_per_index[0]);
    if (captured_per_index[1] !== 8)
      $fatal(1, "captured_per_index[1] was %0d, expected 8",
             captured_per_index[1]);
    $display("All checks passed");
  end
endmodule
