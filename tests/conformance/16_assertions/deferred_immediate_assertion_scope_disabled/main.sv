// @reports-nothing:
//
// Disabling the outermost scope of a procedure that has an active deferred
// assertion queue flushes that queue, and every pending report on it is cleared
// (LRM 16.4.2, 16.4.4). Here the assertion's expression is false where the
// statement is reached, so a report is queued; another procedure disables the
// enclosing block in the same time step, before the report can mature, so a
// conforming tool reports nothing.
module Top;
  logic go;
  logic clear;
  int completed;

  always @(go) begin : b2
    assert #0 (go);
  end

  always @(clear) begin : b3
    disable b2;
  end

  initial begin
    completed = 0;
    go = 1'b0;
    clear = 1'b1;
    #1 completed = 1;
  end

  final begin
    if (completed !== 1) $fatal(1, "the driving procedure did not complete");
    $display("All checks passed");
  end
endmodule
