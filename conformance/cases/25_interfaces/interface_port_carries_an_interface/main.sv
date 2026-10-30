// An interface is instantiated in every position a module is, including inside
// another interface (LRM 25.3), so an interface may itself take an interface
// port. A module connected to the outer one reaches the outer interface's own
// members through its port; which interface the outer one was built against is
// part of what that outer interface is, so two of them bound to different
// interfaces stay distinct objects even though one design element declares
// both.
interface Bus #(
    parameter int W = 8
);
  logic [W-1:0] data;
endinterface

interface Wrap (
    Bus b
);
  int hits;
endinterface

module Leaf (
    Wrap w
);
  initial w.hits = w.hits + 1;
endmodule

module Top;
  Bus #(8) narrow ();
  Bus #(16) wide ();

  Wrap on_narrow (.b(narrow));
  Wrap on_wide (.b(wide));

  Leaf leaf_narrow (.w(on_narrow));
  Leaf leaf_wide (.w(on_wide));

  final begin
    if (on_narrow.hits !== 1)
      $fatal(1, "on_narrow.hits was %0d, expected 1", on_narrow.hits);
    if (on_wide.hits !== 1)
      $fatal(1, "on_wide.hits was %0d, expected 1", on_wide.hits);
    $display("All checks passed");
  end
endmodule
