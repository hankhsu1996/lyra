// One subroutine serves every kind of storage the standard permits as a
// reference actual. A variable, a class property and an automatic variable are
// three of the four categories a ref argument may be passed (LRM 13.5.2), and
// the subroutine states none of them: it declares one formal and is written
// once, so which kind a call hands it is the caller's alone.
//
// They are not alike in what the language says about a write to them. A write
// to a variable of the design hierarchy is a value change a process waiting on
// that variable observes (LRM 9.4.2); a class property and an automatic
// variable carry no such obligation. So the same statement in the same body
// has to raise an event for one actual and not for the others, and what it
// writes has to reach the caller's own storage in every case, immediately and
// before the subroutine returns.
module Top;
  class Holder;
    int owned;
  endclass

  Holder held;
  int scoped;
  int carried;
  int wakes;

  task automatic add_to(ref int slot, input int amount);
    slot = slot + amount;
  endtask

  always @(scoped) wakes = wakes + 1;

  initial begin
    automatic int detached = 300;

    held = new();
    held.owned = 200;
    scoped = 0;
    carried = -1;
    #1;
    wakes = 0;

    add_to(scoped, 7);
    add_to(held.owned, 7);
    add_to(detached, 7);
    carried = detached;
    #1;
  end

  final begin
    if (scoped !== 7) $fatal(1, "scoped was %0d, expected 7", scoped);
    if (held.owned !== 207)
      $fatal(1, "held.owned was %0d, expected 207", held.owned);
    if (carried !== 307) $fatal(1, "carried was %0d, expected 307", carried);
    if (wakes !== 1) $fatal(1, "wakes was %0d, expected 1", wakes);
    $display("All checks passed");
  end
endmodule
