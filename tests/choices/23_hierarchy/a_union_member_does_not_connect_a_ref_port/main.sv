// @rejected:
//
// A 'ref' port is connected to a variable, and references to the port are
// references to what it is connected to (LRM 23.3.3.2). The clause does not
// say whether a part of a variable may stand there; the arguments a reference
// may be bound to are listed, and a member of an unpacked union is not among
// them (LRM 13.5.2). Lyra holds the port to that list, so a union's member is
// refused here as it is as an argument.
module Leaf (ref int r);
  initial #1 r = 3;
endmodule

module Top;
  typedef union { int a; int b; } u_t;
  u_t v;

  Leaf leaf (.r(v.a));

  initial #2 $display("the program the standard rejects ran");
endmodule
