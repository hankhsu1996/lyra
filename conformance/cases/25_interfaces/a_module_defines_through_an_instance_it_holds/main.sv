// A module defines what an interface leaves to it through an interface instance
// it holds, as well as through a port (LRM 25.7). A `disable` of such a task
// through the instance ends the call running it (LRM 9.6.2).
interface Bus;
  extern function int Get();
  extern task Finish();
endinterface

module Top;
  Bus bus ();

  int finished = 0;
  int ended = -1;

  function int bus.Get();
    return 7;
  endfunction

  task bus.Finish();
    #3;
    finished = 1;
  endtask

  initial begin
    fork
      bus.Finish();
      #1 disable bus.Finish;
    join
    ended = $time;
  end

  final begin
    if (bus.Get() !== 7) $fatal(1, "bus.Get() was %0d, expected 7", bus.Get());
    if (finished !== 0) $fatal(1, "finished was %0d, expected 0", finished);
    if (ended !== 1) $fatal(1, "ended was %0d, expected 1", ended);
    $display("All checks passed");
  end
endmodule
