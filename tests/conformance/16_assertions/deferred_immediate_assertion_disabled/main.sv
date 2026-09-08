// @reports-nothing:
//
// Disabling a specific deferred assertion cancels any pending assertion reports
// for it (LRM 16.4.4). A statement label creates a named block around the
// statement it labels (LRM 16.3), so the assertion is named the way any block
// is, and the `disable` naming it withdraws the report that the false
// expression queued before that report could mature. This is the standard's own
// idiom for reporting a failure only in time steps where a condition does not
// settle, so a conforming tool reports nothing here.
module Top;
  logic bad_val;
  logic bad_val_ok;
  int completed;

  always @(bad_val or bad_val_ok) begin : b1
    a1: assert #0 (bad_val);
    if (bad_val_ok) begin
      disable a1;
    end
  end

  initial begin
    completed = 0;
    bad_val = 1'b0;
    bad_val_ok = 1'b1;
    #1 completed = 1;
  end

  final begin
    if (completed !== 1) $fatal(1, "the driving procedure did not complete");
    $display("All checks passed");
  end
endmodule
