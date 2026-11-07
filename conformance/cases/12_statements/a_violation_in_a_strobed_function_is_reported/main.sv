// @reports: violation
//
// With no default to select, a unique-case asserts that some case_item
// matches, so an execution where none does is a violation and is reported
// (LRM 12.5.3), however the statement is reached. A strobed write's arguments
// are evaluated at the end of the time step (LRM 21.2.2), so a function one of
// them calls executes the statement there, and its violation is still
// reported. A tool may end the run at the report, so readiness is printed
// before the write that evaluates it.
module Top;
  function automatic int classify(bit [2:0] value);
    unique case (value)
      3'd0, 3'd1: return 1;
      3'd2: return 2;
    endcase
    return 7;
  endfunction

  initial begin
    $display("Ready for the report");
    $strobe("classified as %0d", classify(3'd5));
    #1 $display("All checks passed");
  end
endmodule
