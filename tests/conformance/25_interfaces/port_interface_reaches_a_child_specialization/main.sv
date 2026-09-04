// The interface a port carries is fixed where the module is instantiated (LRM
// 25.3.3), so everything inside the module that depends on that interface is
// fixed there too -- including a parameter the module computes from it and
// passes to a child it instantiates. Two instantiations bound to differently
// parameterized interfaces therefore build children with different parameters,
// and a port carrying a range settles this exactly as one without a range does.
interface Bus #(
    parameter int W = 8
);
  logic [W-1:0] data;
endinterface

module Probe #(
    parameter int Width = 1
);
  int seen = 0;

  initial #2 seen = Width;
endmodule

module Reader (
    interface a[2]
);
  Probe #(
      .Width($bits(a[0].data))
  ) probe ();
endmodule

module Top;
  Bus #(8) narrow[2] ();
  Bus #(16) wide[2] ();

  Reader on_narrow (.a(narrow));
  Reader on_wide (.a(wide));

  final begin
    if (on_narrow.probe.seen !== 8)
      $fatal(
          1, "on_narrow.probe.seen was %0d, expected 8", on_narrow.probe.seen
      );
    if (on_wide.probe.seen !== 16)
      $fatal(1, "on_wide.probe.seen was %0d, expected 16", on_wide.probe.seen);
    $display("All checks passed");
  end
endmodule
