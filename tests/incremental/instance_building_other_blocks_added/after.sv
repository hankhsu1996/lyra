// @remakes: $root Leaf__16838c6b79aa9556__6d025dd515179a46 Top
//
// One module builds one alternative of a conditional generate for the value it
// is handed (LRM 27.5), and is instantiated once, in a module of its own. A
// second instance is added in the module holding that one, handed a value
// that builds the other alternative. Which blocks an instance built is part of
// its unit's name, so the new instance is a unit of its own, and its value
// names its specialization; the first one keeps its name and what it was
// compiled to, and so does the module holding it. The module the instance is
// written in holds another child, and the design root states how an object of
// the new unit is made.

module Leaf #(parameter int K = 0) (output int o);
  if (K % 2 != 0) begin : parity
    initial o = 1;
  end else begin : parity
    initial o = 2;
  end
endmodule

module Holder;
  int o;
  Leaf #(.K(1)) leaf (.o(o));
endmodule

module Top;
  int o;
  Holder holder ();
  Leaf #(.K(2)) leaf (.o(o));
endmodule
