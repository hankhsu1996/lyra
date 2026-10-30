// LRM 10.6.2: a `force` on a net overrides every driver of that net -- gate
// outputs, module outputs and continuous assignments alike -- until a `release`
// is executed on it, and when released the net is immediately assigned the
// value its drivers determine. The drivers therefore go on driving underneath
// the force rather than being suspended by it: a driver whose value moved while
// the net was forced is what the net answers with the moment it is released.
module Top;
  logic a = 1'b0;
  logic b = 1'b0;
  wire  w;

  assign w = a | b;

  initial begin
    #1;
    if (w !== 1'b0) $fatal(1, "an undriven-low net read %b, expected 0", w);

    force w = 1'b1;
    #1;
    if (w !== 1'b1) $fatal(1, "a forced net read %b, expected 1", w);

    a = 1'b1;
    #1;
    if (w !== 1'b1)
      $fatal(1, "a driver changing under a force moved the net to %b", w);

    a = 1'b0;
    b = 1'b0;
    #1;
    if (w !== 1'b1) $fatal(1, "a forced net stopped reading 1, read %b", w);

    release w;
    #1;
    if (w !== 1'b0)
      $fatal(1, "a released net read %b, expected its drivers' 0", w);

    b = 1'b1;
    #1;
    if (w !== 1'b1)
      $fatal(1, "a released net stopped following its driver, read %b", w);

    $display("All checks passed");
  end
endmodule
