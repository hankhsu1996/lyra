// @top: cfg
//
// A library is a named collection of cells, and a cell is found by its library
// and its name (LRM 33.2.1, 33.3), so two libraries may each hold a cell `A`
// and one design may hold both. Which one an instance is bound to is chosen
// by an instance clause naming it (LRM 33.4.1.6), by the library list set on
// an instance above it, which everything below inherits (LRM 33.4.1.5), by
// the configuration an instance above it is handed to (LRM 33.4.2), or by a
// cell clause (LRM 33.4.1.4). Two instances of one module whose children are
// bound to different cells are built differently, and so are two that a bind
// directive in each of two cells of one name reaches (LRM 23.11).
module H;
  A a ();
endmodule

module Probe (input logic [7:0] d);
  logic [7:0] seen;
  assign seen = d;
endmodule

module T;
endmodule

module TC;
  H h1 (), h2 (), h3 (), h4 ();
  B b ();
  T t1 (), t2 ();
  Binder from_lib1 (), from_lib2 ();

  final begin
    if (h1.a.which !== 1) $fatal(1, "h1.a is lib%0d.A", h1.a.which);
    if (h2.a.which !== 2) $fatal(1, "h2.a is lib%0d.A", h2.a.which);
    if (h3.a.which !== 2) $fatal(1, "h3.a is lib%0d.A", h3.a.which);
    if (h4.a.which !== 2) $fatal(1, "h4.a is lib%0d.A", h4.a.which);
    if (b.which !== 2) $fatal(1, "b is lib%0d.B", b.which);
    if (t1.x.seen !== 8'd1) $fatal(1, "t1.x was handed %0d", t1.x.seen);
    if (t2.x.seen !== 8'd2) $fatal(1, "t2.x was handed %0d", t2.x.seen);
    $display("All checks passed");
  end
endmodule

config below_h4;
  design work.H;
  default liblist work lib2;
endconfig

config cfg;
  design work.TC;
  default liblist work lib1;
  instance TC.h2.a use lib2.A;
  instance TC.h3 liblist work lib2;
  instance TC.h4 use work.below_h4:config;
  cell B use lib2.B;
  instance TC.from_lib2 use lib2.Binder;
endconfig
