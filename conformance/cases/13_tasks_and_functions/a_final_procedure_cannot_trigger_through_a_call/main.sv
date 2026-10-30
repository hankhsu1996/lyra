// @reports: error
//
// Scheduling what cannot become active until a function returns is allowed
// only on a thread an initial or always procedure created, or a fork of one,
// and a tool issues an error otherwise (LRM 13.4.4). The final procedures are
// function calls from a process neither created (LRM 9.2.3), so a nonblocking
// event trigger a function they call makes is an error.
module Top;
  event e;

  function void trigger_later();
    ->> e;
  endfunction

  initial #5;

  final begin
    $display("Ready for the report");
    trigger_later();
    $display("All checks passed");
  end
endmodule
