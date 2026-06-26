module Top;
  logic clk = 1'b0;
  logic a = 1'b0;
  logic b = 1'b1;

  // Module-item concurrent assertion: without --disable-assertions this blocks
  // lowering, and an empty always-loop in its place would be a zero-delay hang.
  // The flag drops the synthesized process entirely.
  assert property (@(posedge clk) a |-> b);

  initial begin
    // Immediate assertion with a deliberately false condition. The flag skips
    // it, so execution falls through to the display rather than stopping.
    assert (a == b);
    $display("RUN");
  end
endmodule
