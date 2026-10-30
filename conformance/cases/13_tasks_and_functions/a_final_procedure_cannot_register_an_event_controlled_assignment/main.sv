// @reports: error
//
// Scheduling what cannot become active until a function returns is allowed
// only on a thread an initial or always procedure created, or a fork of one,
// and a tool issues an error otherwise (LRM 13.4.4). The final procedures are
// function calls from a process neither created (LRM 9.2.3). A nonblocking
// assignment under an event control schedules its update for after the event,
// so making it is the error, though the event never occurs.
module Top;
  event never;
  int x;

  function void schedule();
    x <= @(never) 1;
  endfunction

  initial #5;

  final begin
    $display("Ready for the report");
    schedule();
    $display("All checks passed");
  end
endmodule
