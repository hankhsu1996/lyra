// An event control inside a class method may reach an interface's variable
// through a virtual interface the class holds (LRM 25.9, whose own example
// waits on `@(posedge bus.grant)`). What the wait depends on is the instance
// the virtual interface holds when the control is evaluated (LRM 9.4.2), so a
// transactor handed a different instance waits on that instance's signal and
// is not woken by the other one's. The same holds of a virtual interface a
// module declares, of a variable of an interface the instance instantiates,
// and of a name the handle's modport defines: over storage it is a change to
// that storage, and computed it is a change to any variable the expression
// reads (LRM 25.5.4).
interface Inner;
  logic [7:0] x = 8'h00;
endinterface

interface Bus;
  logic       grant = 1'b0;
  logic [7:0] data = 8'h00;
  Inner       sub ();
  modport view(input .low(data[3:0]), input .plus(data + 8'd1));
endinterface

class Waiter;
  virtual Bus bus;
  time woke = 0;

  function new(virtual Bus b);
    bus = b;
  endfunction

  task automatic Await();
    @(posedge bus.grant);
    woke = $time;
  endtask
endclass

module Top;
  Bus first ();
  Bus second ();
  Waiter on_first;
  Waiter on_second;

  virtual Bus held;
  virtual Bus.view viewed;
  time data_woke = 0;
  time low_woke = 0;
  time plus_woke = 0;
  time nested_woke = 0;

  initial begin
    on_first  = new(first);
    on_second = new(second);
    held = second;
    viewed = second;
    fork
      on_first.Await();
      on_second.Await();
      begin
        @(held.data);
        data_woke = $time;
      end
      begin
        @(viewed.low);
        low_woke = $time;
      end
      begin
        @(viewed.plus);
        plus_woke = $time;
      end
      begin
        @(held.sub.x);
        nested_woke = $time;
      end
    join_none
    #5 second.grant = 1'b1;
    #5 first.grant = 1'b1;
    // Only the high nibble moves, which changes the computed name and not the
    // one designating the low nibble; a write to the instance the handles do
    // not hold wakes neither.
    #5 first.data = 8'h0F;
    #5 second.data = 8'h10;
    #5 second.data = 8'h13;
    #5 second.sub.x = 8'h01;
  end

  final begin
    if (on_second.woke !== 5) $fatal(1, "second's waiter woke at %0t, expected 5", on_second.woke);
    if (on_first.woke !== 10) $fatal(1, "first's waiter woke at %0t, expected 10", on_first.woke);
    if (data_woke !== 20) $fatal(1, "the wait on data woke at %0t, expected 20", data_woke);
    if (plus_woke !== 20) $fatal(1, "the wait on plus woke at %0t, expected 20", plus_woke);
    if (low_woke !== 25) $fatal(1, "the wait on low woke at %0t, expected 25", low_woke);
    if (nested_woke !== 30) $fatal(1, "the wait on sub.x woke at %0t, expected 30", nested_woke);
    $display("All checks passed");
  end
endmodule
