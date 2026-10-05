// @top: cfg
//
// A configuration's instance clause applies to the one instance its
// hierarchical name selects (LRM 33.4.1.3): its use clause binds that instance
// to the cell it names (LRM 33.4.1.6), or sets its parameters (LRM 33.4.3),
// and every other instance of the same module keeps what its own
// instantiation says -- also where the selected instance sits under a parent
// built from a module instantiated more than once. The design statement's
// top-level module is selected the same way (LRM 33.4.3, Example 3).
module Adder #(parameter int W = 8);
  int w = W;
endmodule

module A;
  int which = 1;
endmodule

module B;
  int which = 2;
endmodule

module H;
  A a ();
  Adder add ();
endmodule

module TC #(parameter int TW = 1);
  H h1 (), h2 ();
  int tw = TW;

  final begin
    if (tw !== 5) $fatal(1, "TC.TW was %0d", tw);
    if (h1.a.which !== 1) $fatal(1, "h1.a is cell %0d", h1.a.which);
    if (h2.a.which !== 2) $fatal(1, "h2.a is cell %0d", h2.a.which);
    if (h1.add.w !== 24) $fatal(1, "h1.add.W was %0d", h1.add.w);
    if (h2.add.w !== 8) $fatal(1, "h2.add.W was %0d", h2.add.w);
    $display("All checks passed");
  end
endmodule

config cfg;
  design TC;
  instance TC use #(.TW(5));
  instance TC.h2.a use B;
  instance TC.h1.add use #(.W(24));
endconfig
