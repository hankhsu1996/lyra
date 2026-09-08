// @reports-nothing:
//
// A deferred immediate assertion suppresses false reports from transient
// combinational values (LRM 16.4). Its report is queued when the expression is
// false but matures only in the Observed region, and a process that re-executes
// before then -- an always_comb resuming on one of its dependent signals -- is a
// flush point that clears the queue. Whichever order the continuous assignment
// to not_a and the block settle in, once they have the expression holds and no
// report survives to mature, so a conforming tool reports nothing.
module Top;
  logic a;
  logic not_a;
  int completed;

  assign not_a = ~a;

  always_comb begin : blk
    assert #0 (not_a != a);
  end

  initial begin
    completed = 0;
    a = 0;
    #1 a = 1;
    #1 a = 0;
    completed = 1;
  end

  final begin
    if (completed !== 1) $fatal(1, "the driving procedure did not complete");
    $display("All checks passed");
  end
endmodule
