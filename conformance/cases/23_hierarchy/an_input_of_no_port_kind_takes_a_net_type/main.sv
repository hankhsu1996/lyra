// @rejected:
//
// An input port declared with no port kind is a net of the default net type
// (LRM 23.2.2.3), and a net's data type shall be a 4-state integral type or an
// unpacked aggregate of those (LRM 6.7.1). `int` is 2-state, so a conforming
// tool rejects this program; `input var int` declares the variable port it
// means.
module Leaf (input int a);
  initial $display("the program the standard rejects ran with %0d", a);
endmodule

module Top;
  Leaf u (5);
endmodule
