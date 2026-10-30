// A module declared inside another is named in that module's own name space
// (LRM 23.4, 3.13), so two modules may each declare a module `A`, beside an
// `A` declared outside both, and each instantiation is of the one its scope
// declares. A nested module reads the parameters of the module declaring it
// (LRM 23.9), so the one two specializations declare is not one module.
module A;
  int which = 0;
endmodule

module P;
  module A;
    int which = 1;
  endmodule
  A a ();
endmodule

module Q;
  module A;
    int which = 2;
  endmodule
  A a ();
endmodule

module Sized #(parameter int W = 8);
  module A;
    logic [W-1:0] v;
    int bits = $bits(v);
  endmodule
  A a ();
endmodule

module Top;
  A a ();
  P p ();
  Q q ();
  Sized #(8) s8 ();
  Sized #(16) s16 ();

  final begin
    if (a.which !== 0) $fatal(1, "a is the A of %0d", a.which);
    if (p.a.which !== 1) $fatal(1, "p.a is the A of %0d", p.a.which);
    if (q.a.which !== 2) $fatal(1, "q.a is the A of %0d", q.a.which);
    if (s8.a.bits !== 8) $fatal(1, "s8.a.v has %0d bits", s8.a.bits);
    if (s16.a.bits !== 16) $fatal(1, "s16.a.v has %0d bits", s16.a.bits);
    $display("All checks passed");
  end
endmodule
