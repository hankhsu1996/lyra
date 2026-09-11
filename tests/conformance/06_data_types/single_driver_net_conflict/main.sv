// @error: cannot have multiple drivers
//
// It shall be an error to connect any bit of a uwire net to more than one
// driver, so a conforming tool rejects this program (LRM 6.6.2). The second
// driver reaches the net through a port connection authored in another scope,
// which is where the rule bites in real designs and where it cannot be seen
// from either unit alone.
module Leaf (output uwire [7:0] o);
  assign o = 8'h0F;
endmodule

module Top;
  uwire [7:0] contended;

  Leaf leaf (.o(contended));

  assign contended = 8'hF0;
endmodule
