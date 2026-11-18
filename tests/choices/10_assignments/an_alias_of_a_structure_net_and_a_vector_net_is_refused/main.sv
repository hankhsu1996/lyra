// @rejected:
//
// An alias makes the bits of its members the same physical nets, under the
// bit overlay rules of a packed union with the same member types (LRM 10.11).
// No packed union holds an unpacked structure, so between a net of one and a
// vector net the standard gives the structure's bits no order against the
// vector's, and Lyra refuses the alias. Two nets of one unpacked structure
// type overlay member by member, which no order of their bits changes.
module Top;
  typedef struct {
    logic [3:0] a;
    logic [7:0] b;
  } parts_t;

  wire parts_t s;
  wire logic [11:0] v;
  alias s = v;
  assign v = 12'h123;

  initial #1 $display("the program the standard leaves unsettled ran");
endmodule
