// A virtual interface is a variable holding an interface instance, and what it
// reaches is the instance it holds when the access runs (LRM 25.9). It is
// assigned an instance, another virtual interface of the same type, or `null`,
// and compared against the same three. It may be a class property initialized
// through `new()`, a subroutine argument, or an element of an array selected
// while the simulation runs, and through it a procedural statement reads and
// writes the instance's variables and calls its subroutines. A virtual
// interface selecting a modport reaches what that view offers, and one naming
// no modport may be assigned to it. The type includes the interface's
// parameters, so a member's width is the one the instance was built with.
interface Bus #(
    parameter int W = 8
);
  logic [W-1:0] data;
  int           writes;

  function automatic void Put(input logic [W-1:0] value);
    data   = value;
    writes = writes + 1;
  endfunction

  modport sink(input data);
endinterface

class Driver;
  virtual Bus bus;

  function new(virtual Bus b);
    bus = b;
  endfunction

  task automatic Drive(input logic [7:0] value);
    bus.data = value;
  endtask
endclass

module Top;
  Bus first ();
  Bus second ();
  Bus #(.W(16)) wide ();

  virtual Bus held;
  virtual Bus other;
  virtual Bus.sink viewed;
  virtual Bus #(.W(16)) wide_held;
  virtual Bus pick [2];

  logic [7:0] seen = 8'h00;
  logic [15:0] seen_wide = 16'h0000;
  bit was_null = 1'b0;
  bit held_is_first = 1'b0;
  bit held_is_second = 1'b1;
  bit same_after_copy = 1'b0;

  function automatic void Poke(virtual Bus b, input logic [7:0] value);
    b.Put(value);
  endfunction

  initial begin
    Driver d;

    // Unassigned, a virtual interface holds null (LRM 25.9).
    was_null = (held == null);

    held = first;
    held_is_first = (held == first);
    held_is_second = (held == second);
    held.data = 8'h11;

    other = held;
    same_after_copy = (other == held);
    other = second;
    other.data = 8'h22;

    // No modport selected may be assigned to a modport selected.
    viewed = first;
    seen = viewed.data;

    d = new(second);
    d.Drive(8'h33);

    Poke(first, 8'h44);

    pick[0] = first;
    pick[1] = second;
    // The element is chosen by a value the loop computes, not a constant.
    for (int i = 0; i < 2; i++) pick[i].Put(8'h50 + i);

    wide_held = wide;
    wide_held.data = 16'hbeef;
    seen_wide = wide_held.data;

    held = null;
  end

  final begin
    if (was_null !== 1'b1) $fatal(1, "an unassigned virtual interface was not null");
    if (held_is_first !== 1'b1) $fatal(1, "held did not compare equal to first");
    if (held_is_second !== 1'b0) $fatal(1, "held compared equal to second");
    if (same_after_copy !== 1'b1) $fatal(1, "a copied virtual interface compared unequal");
    if (held !== null) $fatal(1, "held was not null after being assigned null");
    if (seen !== 8'h11) $fatal(1, "seen through the view was %h, expected 11", seen);
    // Written 11 through held, 44 by Poke, then 50 through pick[0].
    if (first.data !== 8'h50) $fatal(1, "first.data was %h, expected 50", first.data);
    // Written 22 through other, 33 by the driver, then 51 through pick[1].
    if (second.data !== 8'h51) $fatal(1, "second.data was %h, expected 51", second.data);
    if (first.writes !== 2) $fatal(1, "first.writes was %0d, expected 2", first.writes);
    if (second.writes !== 1) $fatal(1, "second.writes was %0d, expected 1", second.writes);
    if (wide.data !== 16'hbeef) $fatal(1, "wide.data was %h, expected beef", wide.data);
    if (seen_wide !== 16'hbeef) $fatal(1, "seen_wide was %h, expected beef", seen_wide);
    $display("All checks passed");
  end
endmodule
