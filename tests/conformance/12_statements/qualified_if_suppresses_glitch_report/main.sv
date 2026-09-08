// @reports-nothing:
//
// A unique-if violation report is deferred to the Observed region and cleared
// by a flush point -- an always_comb resuming on a dependent signal -- so a
// transient combinational value that momentarily matches no condition does not
// produce a false report (LRM 12.4.2.1). Whichever order the continuous
// assignment to not_a and the block settle in, once they have exactly one
// condition holds, so a conforming tool reports nothing.
module Top;
  logic a;
  logic not_a;
  logic z;
  int completed;

  assign not_a = ~a;

  always_comb begin : blk
    unique if (a)
      z = 1'b0;
    else if (not_a)
      z = 1'b1;
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
