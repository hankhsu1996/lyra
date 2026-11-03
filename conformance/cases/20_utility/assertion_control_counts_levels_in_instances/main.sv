// An assertion control task's `levels` counts instances below each scope it
// names, as `$dumpvars` counts them (LRM 20.11, 21.7.1.2): the scope named is
// the first level, a generate block is part of the instance holding it and
// counts as no level, 0 reaches every level, and a count below 0 reaches none.
// A call naming no scope counts from each top-level instance. A generate block
// named is the first level as an instance is.
//
// Every assertion here holds, so each check of one runs its pass statement
// once, and a count of those is how many times it was checked.
module Leaf (
    input logic go
);
  int checked = 0;
  always @(go) assert (1) checked = checked + 1;
endmodule

module Mid (
    input logic go
);
  int checked = 0;
  always @(go) assert (1) checked = checked + 1;
  if (1) begin : g
    Leaf leaf (.go);
  end
endmodule

module Top;
  logic go = 0;
  int checked = 0;
  always @(go) assert (1) checked = checked + 1;
  Mid mid (.go);

  int last[3];
  function automatic void Count(
      input int want_top, input int want_mid, input int want_leaf,
      input string step);
    int now[3];
    int want[3];
    now = '{checked, mid.checked, mid.g.leaf.checked};
    want = '{want_top, want_mid, want_leaf};
    for (int i = 0; i < 3; i++) begin
      if (now[i] - last[i] !== want[i]) begin
        $fatal(1, "%s: assertion %0d was checked %0d times, expected %0d",
               step, i, now[i] - last[i], want[i]);
      end
    end
    last = now;
  endfunction

  initial begin
    #1 go = ~go;
    #1 Count(1, 1, 1, "every assertion on");

    // The generate block between `mid` and `leaf` is no level, so two levels
    // from `top` reach `mid` and not `leaf`.
    $assertoff(2, Top);
    #1 go = ~go;
    #1 Count(0, 0, 1, "two levels from the top off");

    $asserton;
    $assertoff(2, mid);
    #1 go = ~go;
    #1 Count(1, 0, 0, "two levels from mid off");

    $asserton;
    $assertoff(-1, mid);
    #1 go = ~go;
    #1 Count(1, 1, 1, "a count below 0 reaches nothing");

    $asserton;
    $assertoff(1);
    #1 go = ~go;
    #1 Count(0, 1, 1, "the tops' own level off");

    // A generate block named is the first level, and `leaf` the second.
    $asserton;
    $assertoff(1, mid.g);
    #1 go = ~go;
    #1 Count(1, 1, 1, "a generate block's own level off");

    $asserton;
    $assertoff(2, mid.g);
    #1 go = ~go;
    #1 Count(1, 1, 0, "two levels from a generate block off");

    $display("All checks passed");
  end
endmodule
