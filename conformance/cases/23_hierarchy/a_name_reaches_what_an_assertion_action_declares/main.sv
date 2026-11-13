// An assertion's label and its action's named blocks are scopes of the
// hierarchy, so a variable the action declares is reached by a hierarchical
// name made of them, from the module that declares it or from any other, as
// LRM 23.6's example reaches `top.m_i.B1.A1.B2.d`; and such a block is named
// by `disable` the same way (LRM 9.6.2). Without a lifetime the variable is
// static (LRM 6.21): one per instance of the module, shared by every run of
// the action, which a name reads and writes and a wait watches like any other
// variable, wherever the assertion stands -- in a module, an interface or a
// generate block. The same holds of every static a body writes: one a task
// declares is one per instance however many calls share it, one written in a
// block of each block a loop counts out is that block's own, and one written
// in a block no run ever enters exists all the same, holding its initial value
// (LRM 6.21, 10.5).
//
// Rising edges at 5 and 15. Each fails in `u` and passes in `v`. The run of
// `u.a.act` begun at 5 counts 1, then 11 at 8; 100 is written at 12; the run
// begun at 15 counts 101 and is disabled at 16, before it adds 10 more.
interface Bus (input logic clk);
  logic low = 0;
  i: assert property (@(posedge clk) low) else begin : seen
    int count;
    count = count + 1;
  end
endinterface

module Leaf (input logic clk, input logic low);
  a: assert property (@(posedge clk) low) else begin : act
    int runs;
    runs = runs + 1;
    #3 runs = runs + 10;
  end

  if (1) begin : g
    ga: assert property (@(posedge clk) low) else begin : gact
      int gruns;
      gruns = gruns + 1;
    end
  end

  for (genvar k = 0; k < 2; k++) begin : lg
    initial begin : each
      static int base = k * 10;
      base = base + 1;
    end
  end

  task bump;
    static int calls = 0;
    calls = calls + 1;
  endtask

  initial begin : p
    #1 bump;
    if (low) bump;
    if (0) begin : dormant
      static int never = 7;
      never = 0;
    end
  end
endmodule

module Top;
  logic clk = 0;
  logic fails = 0;
  logic holds = 1;
  int woke_at = -1;

  Bus bus (.clk(clk));
  Leaf u (.clk(clk), .low(fails));
  Leaf v (.clk(clk), .low(holds));

  t: assert property (@(posedge clk) fails) else begin : tact
    int here;
    here = here + 1;
  end

  initial repeat (4) #5 clk = ~clk;

  initial begin
    @(u.a.act.runs) woke_at = $time;
  end

  initial #12 u.a.act.runs = 100;

  initial #16 disable u.a.act;

  final begin
    if (t.tact.here !== 2)
      $fatal(1, "t.tact.here is %0d, expected 2", t.tact.here);
    if (u.a.act.runs !== 101)
      $fatal(1, "u.a.act.runs is %0d, expected 101", u.a.act.runs);
    if (v.a.act.runs !== 0)
      $fatal(1, "v.a.act.runs is %0d, expected 0", v.a.act.runs);
    if (woke_at !== 5)
      $fatal(1, "a wait on u.a.act.runs woke at %0d, expected 5", woke_at);
    if (u.g.ga.gact.gruns !== 2)
      $fatal(1, "u.g.ga.gact.gruns is %0d, expected 2", u.g.ga.gact.gruns);
    if (v.g.ga.gact.gruns !== 0)
      $fatal(1, "v.g.ga.gact.gruns is %0d, expected 0", v.g.ga.gact.gruns);
    if (bus.i.seen.count !== 2)
      $fatal(1, "bus.i.seen.count is %0d, expected 2", bus.i.seen.count);
    if (u.bump.calls !== 1 || v.bump.calls !== 2)
      $fatal(1, "u.bump.calls is %0d and v.bump.calls %0d, expected 1 and 2",
             u.bump.calls, v.bump.calls);
    if (u.lg[0].each.base !== 1 || u.lg[1].each.base !== 11)
      $fatal(1, "u.lg[0].each.base is %0d and u.lg[1] %0d, expected 1, 11",
             u.lg[0].each.base, u.lg[1].each.base);
    if (u.p.dormant.never !== 7)
      $fatal(1, "u.p.dormant.never is %0d, expected 7", u.p.dormant.never);
    $display("All checks passed");
  end
endmodule
