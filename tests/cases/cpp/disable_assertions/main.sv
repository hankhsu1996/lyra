module Top;
  logic clk = 1'b0;
  logic a = 1'b0;
  logic b = 1'b1;

  // Module-item concurrent assertion: without --disable-assertions this blocks
  // lowering, and an empty always-loop in its place would be a zero-delay hang.
  // The flag drops the synthesized process entirely.
  assert property (@(posedge clk) a |-> b);

  // Named sequence (LRM 16.8) and property (LRM 16.12) declarations, and the
  // LRM 17 checker that packages one. None of them drives the design, so the
  // flag drops each whole.
  sequence Handshake;
    @(posedge clk) a ##1 b;
  endsequence

  property Follows;
    @(posedge clk) a |-> b;
  endproperty

  checker Watch(logic x);
    assert property (@(posedge clk) x);
  endchecker

  Watch watch_inst(a);

  initial begin
    // Immediate assertion with a deliberately false condition. The flag skips
    // it, so execution falls through to the display rather than stopping.
    assert (a == b);
    // LRM 20.12 assertion control. With every assertion gone these have
    // nothing to act on, so the flag makes them no-ops rather than errors.
    $assertoff;
    $assertkill;
    $asserton;
    $assertcontrol(1);
    $display("RUN");
  end
endmodule
