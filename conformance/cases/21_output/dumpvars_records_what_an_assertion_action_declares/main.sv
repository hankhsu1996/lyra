// What a concurrent assertion's action declares is a variable of the design
// like any other: a block of the action is static as the module around it is,
// so a variable it declares without a lifetime is static (LRM 6.21), and the
// assertion's label and the action's named blocks are scopes of the hierarchy
// (LRM 23.6). So `$dumpvars` naming nothing records it, in those scopes (LRM
// 21.7.1.2, 21.7.2.1), for every kind of assertion with an action, in either
// arm, with a label or without, written where the module is or inside a
// procedure, and so does a static written in a block no run enters; a
// variable declared automatic is one per run of the action and is no variable
// of the model. The dump the run writes is held to the one in
// expected/, read as the standard reads a dump.
`timescale 1ns / 1ns
module Top;
  logic clk = 0;
  logic low = 0;
  logic high = 1;

  a: assert property (@(posedge clk) low) else begin : act
    automatic int each;
    int runs;
    each = 1;
    runs = runs + each;
  end

  b: assume property (@(posedge clk) low) else begin : assumed
    int broken;
    broken = broken + 1;
  end

  assert property (@(posedge clk) low) else begin : unlabelled
    int failed;
    failed = failed + 1;
  end

  c: cover property (@(posedge clk) high) begin : hit
    int hits;
    hits = hits + 1;
    begin : inner
      int deep;
      deep = deep + 2;
    end
  end

  initial begin : p
    d: assert property (@(posedge clk) low) else begin : queued
      int pending;
      pending = pending + 1;
    end
  end

  initial begin : q
    if (0) begin : dormant
      static int never = 7;
    end
  end

  initial begin
    $dumpfile("action.vcd");
    $dumpvars;
    #1 clk = 1;
    #1 clk = 0;
    #1 $display("All checks passed");
  end
endmodule
