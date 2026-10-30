// An event expression that is a reference to a simple object handle creates an
// event when a write to that variable is not equal to its previous value (LRM
// 9.4.2). Three things follow, and a case needs all of them to say anything:
// naming a different object resumes whoever waited on the handle, a write that
// leaves the variable naming what it already named is no event at all, and
// writing a property of the object is not a write to the handle -- which is the
// distinction the clause's own example is written around, placing a wait on a
// property beside a wait on the handle that names it.
class Packet;
  int status;
endclass

module Top;
  timeunit 1ns;
  timeprecision 1ns;

  Packet first;
  Packet second;

  time handle_at;
  time handle_again_at;

  initial begin
    handle_at = 99;
    handle_again_at = 99;
  end

  initial begin
    #1;
    @(first);
    handle_at = $time;
    @(first);
    handle_again_at = $time;
  end

  initial begin
    first = new();
    second = new();

    #5;
    first.status = 3;

    #5;
    first = second;

    #5;
    first = second;
  end

  final begin
    if (handle_at !== 10) $fatal(1, "handle_at was %0d, expected 10", handle_at);
    if (handle_again_at !== 99)
      $fatal(1, "a write naming the same object woke a waiter at %0d",
             handle_again_at);
    $display("All checks passed");
  end
endmodule
