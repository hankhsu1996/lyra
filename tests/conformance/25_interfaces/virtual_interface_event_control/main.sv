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
//
// Assigning the handle while a wait is under way moves the wait onto the
// instance it now holds: the assignment reevaluates the expression, so it is
// an event exactly when the new instance's variable already differs, and a
// write to the instance it held before no longer reaches the wait. A wait on
// the handle itself, or on an instance reached through it, ends when the
// handle is assigned a different instance and not when it is assigned the one
// it holds (LRM 9.4.2). An `always_comb` reading a variable through a handle is
// sensitive to the handle, the longest static prefix of what it reads (LRM
// 9.2.2.2.1), so it runs again when the handle is assigned.
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
  Bus third ();
  Bus fourth ();
  Waiter on_first;
  Waiter on_second;

  virtual Bus held;
  virtual Bus.view viewed;
  time data_woke = 0;
  time low_woke = 0;
  time plus_woke = 0;
  time nested_woke = 0;

  virtual Bus moved_equal;
  virtual Bus moved_differs;
  virtual Bus picked;
  virtual Bus nested_by;
  virtual Bus level_handle;
  virtual Bus comb_handle = second;
  logic [7:0] comb_data;
  time moved_equal_woke = 0;
  time moved_differs_woke = 0;
  time picked_woke = 0;
  time nested_by_woke = 0;
  time level_woke = 0;

  always_comb comb_data = comb_handle.data;

  initial begin
    on_first  = new(first);
    on_second = new(second);
    held = second;
    viewed = second;
    moved_equal = second;
    moved_differs = second;
    picked = second;
    nested_by = second;
    level_handle = second;
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
    // Every handle below holds `second`, whose data is now 8'h13.
    fork
      begin
        @(moved_equal.data);
        moved_equal_woke = $time;
      end
      begin
        @(moved_differs.data);
        moved_differs_woke = $time;
      end
      begin
        @(picked);
        picked_woke = $time;
      end
      begin
        @(nested_by.sub);
        nested_by_woke = $time;
      end
      begin
        wait (level_handle.data == 8'h55);
        level_woke = $time;
      end
    join_none
    #5 begin
      third.data = 8'h13;
      fourth.data = 8'h55;
      moved_equal = third;
      moved_differs = first;
    end
    #5 second.data = 8'h20;
    #5 third.data = 8'h14;
    #5 picked = second;
    #5 picked = first;
    #5 nested_by = first;
    #5 level_handle = fourth;
    #5 comb_handle = first;
  end

  final begin
    if (on_second.woke !== 5) $fatal(1, "second's waiter woke at %0t, expected 5", on_second.woke);
    if (on_first.woke !== 10) $fatal(1, "first's waiter woke at %0t, expected 10", on_first.woke);
    if (data_woke !== 20) $fatal(1, "the wait on data woke at %0t, expected 20", data_woke);
    if (plus_woke !== 20) $fatal(1, "the wait on plus woke at %0t, expected 20", plus_woke);
    if (low_woke !== 25) $fatal(1, "the wait on low woke at %0t, expected 25", low_woke);
    if (nested_woke !== 30) $fatal(1, "the wait on sub.x woke at %0t, expected 30", nested_woke);
    if (moved_differs_woke !== 35) $fatal(1, "the wait moved onto a different value woke at %0t, expected 35", moved_differs_woke);
    if (moved_equal_woke !== 45) $fatal(1, "the wait moved onto an equal value woke at %0t, expected 45", moved_equal_woke);
    if (picked_woke !== 55) $fatal(1, "the wait on the handle woke at %0t, expected 55", picked_woke);
    if (nested_by_woke !== 60) $fatal(1, "the wait on a nested instance woke at %0t, expected 60", nested_by_woke);
    if (level_woke !== 65) $fatal(1, "the level wait woke at %0t, expected 65", level_woke);
    if (comb_data !== 8'h0F) $fatal(1, "always_comb read %h through the reassigned handle, expected 0f", comb_data);
    $display("All checks passed");
  end
endmodule
