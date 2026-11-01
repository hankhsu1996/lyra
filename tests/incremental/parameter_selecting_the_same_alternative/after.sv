// @remakes: Top
//
// An instance is handed another value for a parameter that only selects an
// alternative of a conditional generate (LRM 27.5), and the new value selects
// the alternative the old one did. Which blocks an instance built is no part
// of its unit, and the value is handed to the object, so the child's lowering
// comes out as the body it was, and the child keeps its unit and its body, and
// so does the other instance of it.
// The module writing the instantiation hands the child another value.

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
  Leaf #(.K(5)) leaf (.o(o));
endmodule
