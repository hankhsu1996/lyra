// LRM 38.36.1 hands a value-change callback the object's value and does not
// say when it is read where a callback called for one change makes another.
// Lyra reads it as each change is made, and calls what a change made due once
// what was due before it has been called, so each callback is handed the
// changes in the order they were made, each its own value: a callback after
// the one that wrote the variable again is handed 1 and then 2, never 2
// twice.
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context function int place();
  import "DPI-C" context function int calls_of(int which);
  import "DPI-C" context function int seen_by(int which, int call);

  int x = 0;

  initial begin
    if (place() !== 0) $fatal(1, "placing the callbacks failed");
    #1 x = 1;
    #1;
    if (x !== 2) $fatal(1, "x is %0d, expected 2", x);
    for (int which = 0; which < 2; ++which) begin
      if (calls_of(which) !== 2 || seen_by(which, 0) !== 1 ||
          seen_by(which, 1) !== 2)
        $fatal(1, "callback %0d was called %0d times, handed %0d then %0d",
               which, calls_of(which), seen_by(which, 0), seen_by(which, 1));
    end
    $display("All checks passed");
  end
endmodule
