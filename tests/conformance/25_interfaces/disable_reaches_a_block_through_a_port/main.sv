// An interface instance is a scope on the hierarchy like any other (LRM 25.3),
// so a name reaching one through a port ends at its blocks and tasks the way a
// name reaching a module instance does (LRM 9.6.2, 23.6). What the port
// contributes is only how the reach starts: the module was bound to that
// instance, so the disable lands on that instance's own activity and a second
// instance of the same interface, and the scope that owns both, keep theirs.
interface Bus;
  int reached = 0;
  int finished = 0;
  int held = 0;
  int after_hold = 0;

  initial begin : watch
    #1;
    reached = 1;
    #10;
    finished = 1;
  end

  task automatic hold();
    #1;
    held = 1;
    #10;
    after_hold = 1;
  endtask
endinterface

module Stopper (
    Bus b
);
  initial begin
    #2;
    disable b.watch;
    disable b.hold;
  end
endmodule

module Top;
  Bus first ();
  Bus second ();

  Stopper stopper (first);

  initial first.hold();
  initial second.hold();

  final begin
    if (first.reached !== 1)
      $fatal(1, "first.reached was %0d, expected 1", first.reached);
    if (first.finished !== 0)
      $fatal(1, "first.finished was %0d, expected 0", first.finished);
    if (first.held !== 1)
      $fatal(1, "first.held was %0d, expected 1", first.held);
    if (first.after_hold !== 0)
      $fatal(1, "first.after_hold was %0d, expected 0", first.after_hold);

    // The instance nothing named ran both to the end.
    if (second.finished !== 1)
      $fatal(1, "second.finished was %0d, expected 1", second.finished);
    if (second.after_hold !== 1)
      $fatal(1, "second.after_hold was %0d, expected 1", second.after_hold);
    $display("All checks passed");
  end
endmodule
