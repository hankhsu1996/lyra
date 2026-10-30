// A sequence declared in a clocking block is clocked by that block's event
// (LRM 14.13, 16.16), and its names bind where it is declared (LRM 16.8). So a
// sequence an interface's clocking block declares, reached through a port or
// by a hierarchical name, is clocked by the clock of the interface instance it
// was reached in, and two ports bound to instances on two clocks are sampled
// at two rates. The block's event is whatever its clocking event names: an
// edge qualified by `iff` ticks only while the qualifier holds (LRM 9.4.2.3),
// an event ticks when it is triggered (LRM 15.5.2), and a list ticks on any of
// its entries (LRM 9.4.2.1). A module's own block reached by a hierarchical
// name is the same block as reached plainly.
interface I(input logic clk);
  logic a;
  clocking cb @(posedge clk);
    sequence s;
      a;
    endsequence
  endclocking
endinterface

interface Gated(input logic clk, input logic en);
  logic a;
  clocking cb @(posedge clk iff en);
    sequence s;
      a;
    endsequence
  endclocking
endinterface

interface Triggered;
  event tick;
  logic a;
  clocking cb @(tick);
    sequence s;
      a;
    endsequence
  endclocking
endinterface

interface Either(input logic rise, input logic fall);
  logic a;
  clocking cb @(posedge rise or negedge fall);
    sequence s;
      a;
    endsequence
  endclocking
endinterface

module M(I p, I q, Gated g, Triggered t, Either e);
  int passes_p = 0;
  int fails_p = 0;
  int passes_q = 0;
  int fails_q = 0;
  int gated = 0;
  int triggered = 0;
  int either = 0;
  assert property (p.cb.s) passes_p++; else fails_p++;
  assert property (q.cb.s) passes_q++; else fails_q++;
  assert property (g.cb.s) gated++;
  assert property (t.cb.s) triggered++;
  assert property (e.cb.s) either++;
endmodule

module Named;
  int passes = 0;
  assert property (Top.high.cb.s) passes++;
endmodule

module Top;
  logic fast = 0;
  logic slow = 0;
  logic en = 1;
  I high(fast);
  I low(slow);
  Gated gate(fast, en);
  Triggered trig();
  Either both(fast, slow);
  M m(high, low, gate, trig, both);
  Named n();

  logic b = 1;
  int own_passes = 0;
  int own_by_name = 0;
  clocking own @(posedge slow);
    sequence t;
      b;
    endsequence
  endclocking
  assert property (own.t) own_passes++;
  assert property (Top.own.t) own_by_name++;

  initial begin
    high.a = 1;
    low.a = 0;
    gate.a = 1;
    trig.a = 1;
    both.a = 1;
    repeat (4) begin
      #1 fast = 1;
      #1 fast = 0;
      en = 0;
    end
    #1 slow = 1;
    #1 slow = 0;
    repeat (3) #1 ->trig.tick;
    #1;
    if (m.passes_p !== 4 || m.fails_p !== 0)
      $fatal(1, "through p: %0d passed, %0d failed", m.passes_p, m.fails_p);
    if (m.passes_q !== 0 || m.fails_q !== 1)
      $fatal(1, "through q: %0d passed, %0d failed", m.passes_q, m.fails_q);
    if (m.gated !== 1)
      $fatal(1, "a qualified clock ticked %0d times, expected 1", m.gated);
    if (m.triggered !== 3)
      $fatal(1, "an event clock ticked %0d times, expected 3", m.triggered);
    if (m.either !== 5)
      $fatal(1, "a listed clock ticked %0d times, expected 5", m.either);
    if (n.passes !== 4)
      $fatal(1, "by a hierarchical name: %0d passed", n.passes);
    if (own_passes !== 1)
      $fatal(1, "the module's own clocking block passed %0d times", own_passes);
    if (own_by_name !== 1)
      $fatal(1, "the module's own block by name passed %0d times",
             own_by_name);
    $display("All checks passed");
  end
endmodule
