// A name a modport defines is reached as any component of the instance is, by
// the dot notation through a virtual interface or through a port (LRM 25.5.4,
// 25.9), and stands for what the view wrote there. Where the view joins several
// declarations, every form of read and write the name allows acts on all of
// them, the handle is evaluated once whatever the form does with it, and an
// update lands on the instance the handle held when the statement ran (LRM
// 10.4.2). The handle may stand as an element chosen by a call, a class
// property, or a variable reaching an interface below the one it holds. Reached
// through a port, a name standing for one whole declaration is that
// declaration: a drive of a scalar net takes its rise and fall delays (LRM
// 10.3.3), and an input connected to it reads it. A name a view offers for
// reading is the value its expression computes: a select of it selects that
// value, and an event control on it waits for that value to change (LRM 9.4.2).
typedef union tagged {
  int  number;
  void nothing;
} shape_t;

interface Bus;
  logic [3:0] hi, lo;
  logic [7:0] data;
  logic [7:0] lanes [2];
  int         at = 1;
  wire        line;
  logic [7:0] held = 8'h0f;
  int         queue [$];
  shape_t     choice;
  byte        bytes [4];
  int         evaluations = 0;
  int         counted = 0;

  function automatic int next();
    evaluations++;
    return counted;
  endfunction

  modport ctrl(output .Pair({hi, lo}), output .Swapped({lo, hi}),
               output .Low(data[3:0]), output .Lane(lanes[1]),
               input .Wide(lanes[at]), input .Sum(hi + lo),
               output .Line(line), output .Held(held), input .Queue(queue),
               input .Choice(choice), input .Bytes(bytes),
               input .Counted(next()));
endinterface

module Reader(input logic [7:0] seen);
  initial #1 if (seen !== 8'h0f) $fatal(1, "an input read %h", seen);
endmodule

module Driver(Bus.ctrl p);
  logic drive = 1'b0;
  assign #(3, 4, 1) p.Line = drive;
  Reader reader(.seen(p.Held));

  // A change to x takes the least of the delays on a scalar net, where a
  // vector's would take the rise delay.
  initial begin
    #10 drive = 1'bx;
    #2 if (p.Line !== 1'bx) $fatal(1, "a scalar's x took more than 1");
  end

  int first, last, number;
  int streamed;

  initial begin
    #1;
    first = p.Queue[0];
    last = p.Queue[$];
    number = p.Choice.number;
    streamed = {<<8{p.Bytes with [0:1]}};
    if (first !== 7 || last !== 8) $fatal(1, "a value's queue read %0d %0d", first, last);
    if (number !== 5) $fatal(1, "a value's union read %0d", number);
    if (streamed !== 32'h0201_0000) $fatal(1, "a value's stream read %h", streamed);
  end

  int total = 0;

  initial begin
    #1;
    foreach (p.Queue[i]) total += p.Queue[i];
    if (total !== 15) $fatal(1, "a foreach over a value read %0d", total);
    {<<4{p.Swapped}} = 8'h12;
    if (p.Swapped !== 8'h21) $fatal(1, "a stream through a port read %h", p.Swapped);
  end

  initial begin
    @(p.Counted);
    #1 p.Pair = 8'hee;
  end
endmodule

interface Outer;
  Bus inner ();
endinterface

class Holder;
  virtual Bus.ctrl vif;
endclass

module Top;
  Bus one ();
  Bus two ();
  Bus driven ();
  Driver driver(driven);

  initial begin
    driven.queue.push_back(7);
    driven.queue.push_back(8);
    driven.choice = tagged number 5;
    driven.bytes[0] = 1;
    driven.bytes[1] = 2;
    #2 driven.counted = 1;
  end
  Outer outer ();
  virtual Bus.ctrl view;
  virtual Bus.ctrl views [2];
  virtual Outer around;
  Holder holder;
  int calls = 0;
  logic [7:0] read_pair, read_part, read_sum, read_wide, read_stale;
  logic [7:0] read_nested;
  logic [7:0] after_wait;

  function automatic int pick();
    calls++;
    return 1;
  endfunction

  task automatic bump(inout logic [7:0] x);
    x = x + 1;
  endtask

  task automatic fill(output logic [7:0] x);
    x = 8'h5a;
  endtask

  initial begin
    views[0] = one;
    views[1] = two;
    view = one;

    // A read, and a read of a select of the name.
    view.Pair = 8'h12;
    read_pair = view.Pair;
    read_part = view.Pair[7:4];
    if (one.hi !== 4'h1 || one.lo !== 4'h2)
      $fatal(1, "a write reached %h%h", one.hi, one.lo);

    // A view joining in another order is a view of its own.
    view.Swapped = 8'h34;
    if (one.lo !== 4'h3 || one.hi !== 4'h4)
      $fatal(1, "a swapped write reached %h%h", one.hi, one.lo);

    // An assignment operator and an increment read and write the same parts.
    view.Pair = 8'h0f;
    view.Pair += 1;
    if (one.hi !== 4'h1 || one.lo !== 4'h0)
      $fatal(1, "+= reached %h%h", one.hi, one.lo);
    view.Pair++;
    if (one.lo !== 4'h1) $fatal(1, "++ reached %h", one.lo);

    // Actuals of a task.
    view.Pair = 8'h20;
    bump(view.Pair);
    if (one.lo !== 4'h1) $fatal(1, "inout reached %h", one.lo);
    fill(view.Pair);
    if (one.hi !== 4'h5 || one.lo !== 4'ha)
      $fatal(1, "output reached %h%h", one.hi, one.lo);

    // A member of a larger target.
    {view.Pair, view.Low} = 12'h6b7;
    if (one.hi !== 4'h6 || one.lo !== 4'hb || one.data[3:0] !== 4'h7)
      $fatal(1, "a nested join reached %h%h %h", one.hi, one.lo, one.data);

    // Names of one part, and names computed where the view reads them.
    view.Lane = 8'h77;
    one.at = 1;
    read_wide = view.Wide;
    one.hi = 4'h2;
    one.lo = 4'h3;
    read_sum = view.Sum;

    // The operand the source wrote once runs once.
    views[pick()].Pair = 8'h3c;
    if (calls !== 1) $fatal(1, "a write called pick %0d times", calls);
    if (two.hi !== 4'h3 || two.lo !== 4'hc)
      $fatal(1, "an element's write reached %h%h", two.hi, two.lo);
    read_pair = views[pick()].Pair;
    if (calls !== 2) $fatal(1, "a read called pick %0d times", calls);
    views[pick()].Pair += 1;
    if (calls !== 3) $fatal(1, "+= called pick %0d times", calls);
    views[pick()].Pair <= 8'h99;
    if (calls !== 4) $fatal(1, "<= called pick %0d times", calls);
    #1;
    if (two.hi !== 4'h9 || two.lo !== 4'h9)
      $fatal(1, "an element's update reached %h%h", two.hi, two.lo);

    // An update is deferred and lands on the instance held when it ran.
    view = one;
    one.hi = 4'h0;
    one.lo = 4'h0;
    view.Pair <= 8'hab;
    view = two;
    read_stale = {one.hi, one.lo};
    #1;
    if (read_stale !== 8'h00) $fatal(1, "an update landed at once");
    if (one.hi !== 4'ha || one.lo !== 4'hb)
      $fatal(1, "an update reached %h%h", one.hi, one.lo);

    // A blocking delay reads its target when the delay ends (LRM 10.4.1).
    fork
      #1 view = one;
    join_none
    view.Pair = #2 8'hcd;
    if (one.hi !== 4'hc || one.lo !== 4'hd)
      $fatal(1, "a delayed write reached %h%h", one.hi, one.lo);

    // A wait watches what the name reads.
    view = one;
    fork
      #1 one.lo = 4'h9;
    join_none
    @(view.Pair);
    after_wait = view.Pair;
    if (after_wait !== 8'hc9) $fatal(1, "a wait saw %h", after_wait);

    // A class property as the handle.
    holder = new;
    holder.vif = two;
    holder.vif.Pair = 8'h44;
    if (two.hi !== 4'h4 || two.lo !== 4'h4)
      $fatal(1, "a property's write reached %h%h", two.hi, two.lo);

    // A stream unpacks into what the name joins.
    view = one;
    {<<4{view.Pair}} = 8'h12;
    if (one.hi !== 4'h2 || one.lo !== 4'h1)
      $fatal(1, "a stream reached %h%h", one.hi, one.lo);

    // A view of an interface below the one the handle holds.
    around = outer;
    around.inner.ctrl.Pair = 8'h56;
    read_nested = around.inner.ctrl.Pair;
    if (outer.inner.hi !== 4'h5 || outer.inner.lo !== 4'h6)
      $fatal(1, "a nested view reached %h%h", outer.inner.hi, outer.inner.lo);

    if (read_part !== 8'h01) $fatal(1, "a select read %h", read_part);
    if (read_wide !== 8'h77) $fatal(1, "a computed name read %h", read_wide);
    if (read_sum !== 8'h05) $fatal(1, "a computed sum read %h", read_sum);
    if (read_nested !== 8'h56) $fatal(1, "a nested read %h", read_nested);
    #20;
    if (driven.evaluations === 0) $fatal(1, "a wait evaluated nothing");
    if (driven.hi !== 4'he) $fatal(1, "a wait on a value never woke");
    $display("All checks passed");
  end
endmodule
