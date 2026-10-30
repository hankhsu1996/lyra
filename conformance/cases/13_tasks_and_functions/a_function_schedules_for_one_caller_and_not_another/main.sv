// @reports: error
//
// Whether a function may schedule what cannot become active until it returns
// is decided by the thread calling it, not by the function (LRM 13.4.4): the
// same function schedules for an initial procedure, and is an error once a
// continuous assignment calls it.
module Top;
  bit go;
  int x, y;

  function int schedule_when(bit when);
    if (when) x <= 1;
    return 0;
  endfunction

  assign y = schedule_when(go);

  initial begin
    x = 5;
    #1;
    void'(schedule_when(1));
    #1;
    if (x !== 1) $fatal(1, "x was %0d", x);
    $display("Ready for the report");
    go = 1;
    #1;
    $display("All checks passed");
  end
endmodule
