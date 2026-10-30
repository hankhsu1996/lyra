// @reports: error
//
// Scheduling what cannot become active until a function returns is allowed
// only on a thread an initial or always procedure created, or a fork of one,
// and a tool issues an error otherwise (LRM 13.4.4). A static variable's
// initializer runs before any procedure starts, on no such thread, so a
// fork-join_none in the function it calls is an error; the clause's own example
// marks this declaration illegal.
module Top;
  function automatic int start();
    $display("Ready for the report");
    fork
      $display("a process spawned while initializing");
    join_none
    return 1;
  endfunction

  static int started = start();

  initial #1 $display("All checks passed");
endmodule
