// An interface may declare a subroutine it leaves to a module connected to it
// (LRM 25.7): an `extern` prototype, or a modport `export` that implies one. The
// module defines it through its port by a hierarchical name, and a call on the
// interface runs that definition on the defining module's instance, reading and
// writing that instance's own state. The call is made the same way whoever
// makes it: through another module's port, by the interface itself, and by a
// hierarchical name from the scope that owns the instance. A task's output
// formal comes back to the caller, and the task suspends its caller until it
// completes (LRM 13.3).
interface Bus;
  int seen;

  extern function int Get(input int add);
  modport provider(
      ref seen,
      export Get,
      export task Put(input int value, output int previous)
  );
  modport consumer(
      import Get,
      import task Put(input int value, output int previous)
  );

  // Asked of the interface itself, the call still runs the module's definition.
  function automatic int Twice();
    return Get(0) + Get(0);
  endfunction
endinterface

module Provider (
    Bus.provider b
);
  int k = 7;
  int calls = 0;

  function int b.Get(input int add);
    calls = calls + 1;
    return k + add;
  endfunction

  // Defined through the modport's export alone, with no `extern` in the
  // interface.
  task b.Put(input int value, output int previous);
    #2;
    previous = k;
    k = value;
    b.seen = b.seen + 1;
  endtask
endmodule

module Consumer (
    Bus.consumer b
);
  int got = 0;
  int previous = 0;
  time done = 0;

  initial begin
    #1 got = b.Get(3);
    b.Put(20, previous);
    done = $time;
  end
endmodule

module Top;
  Bus bus ();
  Provider p (bus.provider);
  Consumer c (bus.consumer);

  int direct = 0;
  int twice = 0;

  initial begin
    #10 direct = bus.Get(1);
    twice = bus.Twice();
  end

  final begin
    if (c.got !== 10) $fatal(1, "c.got was %0d, expected 10", c.got);
    if (c.previous !== 7)
      $fatal(1, "c.previous was %0d, expected 7", c.previous);
    if (c.done !== 3) $fatal(1, "c.done was %0t, expected 3", c.done);
    if (p.k !== 20) $fatal(1, "p.k was %0d, expected 20", p.k);
    if (bus.seen !== 1) $fatal(1, "bus.seen was %0d, expected 1", bus.seen);
    if (direct !== 21) $fatal(1, "direct was %0d, expected 21", direct);
    if (twice !== 40) $fatal(1, "twice was %0d, expected 40", twice);
    // One call from the consumer, one by hierarchical name and two through
    // the interface's own function, each counted where the definition lives.
    if (p.calls !== 4) $fatal(1, "p.calls was %0d, expected 4", p.calls);
    $display("All checks passed");
  end
endmodule
