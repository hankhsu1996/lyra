// @remakes: $root Leaf__6d025dd515179a46 Leaf__c111be714ad7c4ed Top
//
// One module builds one alternative of a conditional generate for the value it
// is handed (LRM 27.5), and is instantiated once, in a module of its own. A
// second instance is added in the module holding that one, handed a value
// that builds the other alternative. Which blocks an instance built is no part
// of its unit, so the new instance is one more specialization of the same
// unit, lowered to a body of its own because it built another block; the unit
// now publishes that block's class too, so the first instance's body, which
// states every class of its unit, is made again, while the unit's name, and so
// the module holding the first instance, stay as they were. The module the
// instance is written in holds another child, and the design root states how
// an object of the new specialization is made.

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

endmodule
