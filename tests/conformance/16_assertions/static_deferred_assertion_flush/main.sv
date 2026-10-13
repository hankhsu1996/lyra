// @reports-nothing:
//
// A deferred assertion written outside procedural code is treated as if it
// were contained in an always_comb procedure (LRM 16.4.3). So it is evaluated
// at time zero and again whenever what it reads changes, and its procedure
// resuming on such a change is a flush point (LRM 16.4.2) that clears a report
// still pending from a value that did not last. Whichever order the continuous
// assignment to not_a and these procedures settle in, once they have every
// expression holds and no report survives to mature, so a conforming tool
// reports nothing -- for each deferred form, labelled or not, and in a
// generate block or an interface as in the module itself.
interface Watch (input logic a, input logic not_a);
  assert #0 (not_a != a);
endinterface

module Top;
  logic a;
  logic not_a;
  int completed;

  assign not_a = ~a;

  Watch watch (.a(a), .not_a(not_a));

  assert #0 (not_a != a);
  observed: assert #0 (not_a != a);
  assert final (not_a != a);
  assume #0 (not_a != a);

  if (1) begin : g
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
