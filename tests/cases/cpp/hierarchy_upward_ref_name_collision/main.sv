// An upward reference climbs to the ancestor identified by its module
// definition name. A nearer ancestor whose *instance* name happens to equal
// that module name (here a Decoy instance literally named "M") must not shadow
// the real target: `outer.s` resolves to `outer` (definition M), not to the
// closer instance called "M". LRM 23.8.
module Leaf;
  int x;
  always_comb x = outer.s;
endmodule

module Decoy;
  int s;
  Leaf leaf();
endmodule

module Wrap;
  Decoy M();
endmodule

module M;
  int s;
  Wrap w();
endmodule

module Top;
  M outer();
  initial begin
    outer.w.M.s = 7;
    outer.s = 100;
    #1;
    $display("%0d", outer.w.M.leaf.x);
  end
endmodule
