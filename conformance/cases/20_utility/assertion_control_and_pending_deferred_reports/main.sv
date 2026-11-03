// A deferred assertion's report waits on a queue until it matures, later in
// the time step (LRM 16.4.1). Turning the assertion off leaves a report
// already queued to mature, and stops new checking; killing it flushes the
// queued report, and turning it on again before the report would have matured
// does not bring it back (LRM 20.11).
//
// The assertion fails wherever it is checked, and its fail statement counts
// the reports that matured. Each procedure resumed by `go` checks it in the
// Active region, so the controlling procedure acts on the queued report after
// a `#0`, which runs it later in the same time step and before the report
// matures in the Observed region.
module Top;
  logic go = 0;
  int failed = 0;

  function automatic void Failed();
    failed = failed + 1;
  endfunction

  always @(go) begin
    d : assert #0 (0)
    else Failed();
  end

  initial begin
    #1 go = ~go;
    #0 $assertoff;
    #1;
    if (failed !== 1) begin
      $fatal(1, "off withdrew a queued report: %0d matured, expected 1",
             failed);
    end

    go = ~go;
    #1;
    if (failed !== 1) begin
      $fatal(1, "off still checked: %0d matured, expected 1", failed);
    end

    $asserton;
    go = ~go;
    #0 $assertkill;
    $asserton;
    #1;
    if (failed !== 1) begin
      $fatal(1, "kill left a queued report: %0d matured, expected 1", failed);
    end

    go = ~go;
    #1;
    if (failed !== 2) begin
      $fatal(1, "on again checked nothing: %0d matured, expected 2", failed);
    end
    $display("All checks passed");
  end
endmodule
