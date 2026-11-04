// A port of an interface is connected to storage outside it (LRM 23.3.3), and
// every component of the instance a virtual interface holds is available
// through it (LRM 25.9), its ports among them. So a wait on a port reached
// through a handle waits on what the port is connected to, as a wait on the
// port reached through the hierarchy does: a change there wakes it, and a
// change to the storage another instance's port is connected to does not
// (LRM 9.4.2). That holds whatever the port is -- a variable input, a net
// input, a `ref` port, an output -- whether the wait is an edge, a change or
// a level, whether it is made in a class's task, a module's procedure or a
// process a fork starts, through the handle, a local copy of it or an element
// of an array of them, and through a name the handle's modport lists or one it
// defines over the port (LRM 25.5.4).
//
// A force on the port acts on the port and not on what it is connected to
// (LRM 10.6.2), and a wait on the port through a handle follows it as one
// through the hierarchy does: the force is an event for the wait, a change of
// the connected storage while the force holds is not, and the release, which
// shows that storage again, is one where the value moves.
interface Pins (
    input logic clk,
    input wire [7:0] bus,
    ref logic [7:0] shared,
    output logic [7:0] back
);
  modport view(input clk, input .doubled({bus, bus}));
endinterface

class Waiter;
  virtual Pins pins;
  time rose = 0;

  function new(virtual Pins p);
    pins = p;
  endfunction

  task automatic AwaitEdge();
    @(posedge pins.clk);
    rose = $time;
  endtask
endclass

module Top;
  logic       clk_a = 1'b0;
  logic       clk_b = 1'b0;
  logic [7:0] bus_a = 8'h00;
  logic [7:0] bus_b = 8'h00;
  logic [7:0] shared_a = 8'h00;
  logic [7:0] shared_b = 8'h00;
  logic [7:0] back_a;
  logic [7:0] back_b;

  Pins first (
      .clk(clk_a),
      .bus(bus_a),
      .shared(shared_a),
      .back(back_a)
  );
  Pins second (
      .clk(clk_b),
      .bus(bus_b),
      .shared(shared_b),
      .back(back_b)
  );

  Waiter on_first;
  Waiter on_second;
  virtual Pins held = second;
  virtual Pins.view viewed = second;
  virtual Pins many[2];

  time by_level = 0;
  time by_net = 0;
  time by_ref = 0;
  time by_output = 0;
  time by_local = 0;
  time by_view = 0;
  time by_view_name = 0;
  time by_element = 0;
  time by_force = 0;
  time by_release = 0;
  int  edges = 0;

  always @(posedge held.clk) edges++;

  task automatic AwaitThroughCopy(virtual Pins source);
    virtual Pins copy = source;
    @(copy.bus);
    by_local = $time;
  endtask

  initial begin
    on_first = new(first);
    on_second = new(second);
    many[0] = first;
    many[1] = second;
    fork
      on_first.AwaitEdge();
      on_second.AwaitEdge();
      begin
        wait (held.clk == 1'b1);
        by_level = $time;
      end
      begin
        @(held.bus);
        by_net = $time;
      end
      begin
        @(held.shared);
        by_ref = $time;
      end
      begin
        @(held.back);
        by_output = $time;
      end
      AwaitThroughCopy(second);
      begin
        @(viewed.clk);
        by_view = $time;
      end
      begin
        @(viewed.doubled);
        by_view_name = $time;
      end
      begin
        @(many[1].bus);
        by_element = $time;
      end
    join_none
    // The first instance's ports change first, which wakes nothing waiting
    // through a handle that holds the second.
    #5 begin
      clk_a = 1'b1;
      bus_a = 8'h01;
      shared_a = 8'h01;
      first.back = 8'h01;
    end
    #5 clk_b = 1'b1;
    #5 bus_b = 8'h02;
    #5 shared_b = 8'h03;
    #5 second.back = 8'h04;
    #5 clk_b = 1'b0;
    #5 clk_b = 1'b1;
    // The port is now 1. A force to 0 is an event for a wait on it; the
    // connected variable moving under the force is not; the release shows it
    // again at 1, which is.
    fork
      begin
        @(held.clk);
        by_force = $time;
        @(held.clk);
        by_release = $time;
      end
    join_none
    #5 force second.clk = 1'b0;
    #5 clk_b = 1'b0;
    #5 clk_b = 1'b1;
    #5 release second.clk;
  end

  final begin
    if (on_first.rose !== 5) $fatal(1, "first's waiter woke at %0t, expected 5", on_first.rose);
    if (on_second.rose !== 10) $fatal(1, "second's waiter woke at %0t, expected 10", on_second.rose);
    if (by_level !== 10) $fatal(1, "the level wait woke at %0t, expected 10", by_level);
    if (by_view !== 10) $fatal(1, "the wait on the modport's port woke at %0t, expected 10", by_view);
    if (by_net !== 15) $fatal(1, "the wait on the net port woke at %0t, expected 15", by_net);
    if (by_local !== 15) $fatal(1, "the wait through a copy of the handle woke at %0t, expected 15", by_local);
    if (by_view_name !== 15) $fatal(1, "the wait on the modport's name woke at %0t, expected 15", by_view_name);
    if (by_ref !== 20) $fatal(1, "the wait on the ref port woke at %0t, expected 20", by_ref);
    if (by_output !== 25) $fatal(1, "the wait on the output port woke at %0t, expected 25", by_output);
    if (by_element !== 15) $fatal(1, "the wait through an array element woke at %0t, expected 15", by_element);
    if (by_force !== 40) $fatal(1, "the wait on the forced port woke at %0t, expected 40", by_force);
    if (by_release !== 55) $fatal(1, "the wait on the released port woke at %0t, expected 55", by_release);
    if (edges !== 3) $fatal(1, "the procedure saw %0d edges, expected 3", edges);
    $display("All checks passed");
  end
endmodule
