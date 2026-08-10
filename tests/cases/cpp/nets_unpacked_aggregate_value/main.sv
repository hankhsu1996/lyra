// A net whose data type is an unpacked aggregate (LRM 6.7.1): a net's data type
// may be a fixed-size unpacked array, structure, or union whose every element is
// itself a valid net data type. That makes such a value one net whose bits each
// resolve, not a collection of separate nets, so it installs one undriven value,
// takes one driver per connection, and reads element-wise or member-wise like any
// other aggregate. An unpacked-array port declared with no data type is such a
// net implicitly (LRM 6.10, 23.2.2.3); `wide_i` spells the same shape explicitly,
// with a packed element.
typedef struct {
  logic [3:0] tag;
  logic [7:0] payload;
} frame_t;

// LRM 7.3: only one member of an unpacked union is live at a time, and the
// declared default is its first member.
typedef union {
  logic [3:0] narrow;
  logic [7:0] wide;
} slot_t;

module Child (
    input req_i [0:1],
    input wire logic [7:0] wide_i [0:1],
    output ack_o [0:1],
    output wire logic [7:0] sum_o
);
  assign ack_o = '{req_i[1], req_i[0]};
  assign sum_o = wide_i[0] + wide_i[1];
endmodule

// The same shape left unconnected: with no driver the net holds its type's
// undriven value throughout, one element at a time (LRM 6.6.1).
module Floating (
    input probe_i [0:1]
);
  initial begin
    #1;
    $display("undriven=%b%b", probe_i[0], probe_i[1]);
  end
endmodule

module Top;
  logic req[0:1];
  logic [7:0] wide[0:1];
  logic ack[0:1];
  wire logic [7:0] sum;

  // A struct net driven whole, and one whose members are driven by separate
  // assignments -- each of those drivers contributing high-impedance outside the
  // member it drives, so the two compose.
  wire frame_t whole;
  wire frame_t split;

  // A union net driven on a member other than its declared default. The fold
  // reaches it because a contribution that drives nothing is the identity
  // whichever member it nominally carries.
  wire slot_t chosen;

  assign whole = '{4'h6, 8'hc3};
  assign split.tag = 4'h9;
  assign split.payload = 8'h5a;
  assign chosen.wide = 8'he1;

  Child u (
      .req_i (req),
      .wide_i(wide),
      .ack_o (ack),
      .sum_o (sum)
  );
  Floating f ();

  initial begin
    req[0] = 1'b1;
    req[1] = 1'b0;
    wide[0] = 8'd11;
    wide[1] = 8'd22;
    #1;
    $display("swapped=%b%b", ack[0], ack[1]);
    $display("total=%0d", sum);
    $display("whole=%h%h", whole.tag, whole.payload);
    $display("split=%h%h", split.tag, split.payload);
    $display("chosen=%h", chosen.wide);
  end
endmodule
