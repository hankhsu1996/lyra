// An input port declared with no data type is a net (LRM 23.2.2.3), driven by
// the continuous assignment its connection implies (LRM 23.3.3). Connected to
// the whole of a variable or of a net of its own type, it carries that object's
// value, every change of which is a change of the port, and its first value at
// time zero is a change from the high impedance of a net nothing has driven
// yet (LRM 4.9.1, 9.4.2 Table 9-2). Handed on to another such port, the value
// is the same at every level, and so it is where the port in between is
// declared as a variable (LRM 23.3.3.2). A force on the port overrides the
// port and whatever it is handed on to, never what drives it, and its release
// shows what drives it again, with whatever that moved to meanwhile; a force
// on the source reaches the port as the assignment carries it (LRM 10.6.2). A
// connection to a part of a variable carries that part.
module Leaf (
    input [3:0] in
);
  int changes;
  int rises;
  always @(in) changes++;
  always @(posedge in[0]) rises++;
endmodule

module Mid (
    input [3:0] in
);
  Leaf leaf (.in(in));
endmodule

module TypedMid (
    input logic [3:0] in
);
  Leaf leaf (.in(in));
endmodule

typedef struct packed {
  logic [3:0] tag;
  logic [3:0] payload;
} frame_t;

module Top;
  logic [3:0] a = 4'b0001;
  logic [3:0] b;
  wire [3:0] w;
  frame_t r;
  assign w = b;

  Leaf direct (.in(a));
  Mid chain (.in(a));
  Leaf from_net (.in(w));
  Leaf forced (.in(a));
  Leaf from_member (.in(r.tag));
  TypedMid typed_chain (.in(a));

  logic [3:0] typed_first, typed_forced, typed_released;
  logic [3:0] direct_first, chain_first, net_first;
  int rises_first, changes_first;
  logic [3:0] direct_moved, chain_moved;
  int changes_moved, changes_same;
  logic [3:0] forced_seen, source_under_force, forced_held, direct_under_force;
  logic [3:0] forced_released;
  logic [3:0] chain_forced, chain_held, chain_released;
  logic [3:0] source_forced;
  logic [3:0] net_moved, member_seen;

  initial begin
    b = 4'b0110;
    r = '{tag: 4'h9, payload: 4'h5};
    #1;
    direct_first = direct.in;
    chain_first = chain.leaf.in;
    net_first = from_net.in;
    rises_first = direct.rises;
    changes_first = direct.changes;
    member_seen = from_member.in;
    typed_first = typed_chain.leaf.in;

    a = 4'b0011;
    #1;
    direct_moved = direct.in;
    chain_moved = chain.leaf.in;
    changes_moved = direct.changes;

    // Writing the value the source already holds is no change of the port.
    a = 4'b0011;
    #1;
    changes_same = direct.changes;

    force forced.in = 4'b1111;
    #1;
    forced_seen = forced.in;
    source_under_force = a;
    a = 4'b0100;
    #1;
    forced_held = forced.in;
    direct_under_force = direct.in;
    release forced.in;
    #1;
    forced_released = forced.in;

    force chain.in = 4'b1010;
    #1;
    chain_forced = chain.leaf.in;
    a = 4'b0111;
    #1;
    chain_held = chain.leaf.in;
    release chain.in;
    #1;
    chain_released = chain.leaf.in;

    force typed_chain.in = 4'b1100;
    #1;
    typed_forced = typed_chain.leaf.in;
    release typed_chain.in;
    #1;
    typed_released = typed_chain.leaf.in;

    force a = 4'b0101;
    #1;
    source_forced = direct.in;
    release a;

    b = 4'b1001;
    #1;
    net_moved = from_net.in;
  end

  final begin
    if (direct_first !== 4'b0001)
      $fatal(1, "direct_first was %b, expected 0001", direct_first);
    if (chain_first !== 4'b0001)
      $fatal(1, "chain_first was %b, expected 0001", chain_first);
    if (net_first !== 4'b0110)
      $fatal(1, "net_first was %b, expected 0110", net_first);
    if (rises_first !== 1)
      $fatal(1, "rises_first was %0d, expected 1", rises_first);
    if (changes_first !== 1)
      $fatal(1, "changes_first was %0d, expected 1", changes_first);
    if (member_seen !== 4'h9)
      $fatal(1, "member_seen was %h, expected 9", member_seen);
    if (direct_moved !== 4'b0011)
      $fatal(1, "direct_moved was %b, expected 0011", direct_moved);
    if (chain_moved !== 4'b0011)
      $fatal(1, "chain_moved was %b, expected 0011", chain_moved);
    if (changes_moved !== 2)
      $fatal(1, "changes_moved was %0d, expected 2", changes_moved);
    if (changes_same !== 2)
      $fatal(1, "changes_same was %0d, expected 2", changes_same);
    if (forced_seen !== 4'b1111)
      $fatal(1, "forced_seen was %b, expected 1111", forced_seen);
    if (source_under_force !== 4'b0011)
      $fatal(1, "source_under_force was %b, expected 0011",
             source_under_force);
    if (forced_held !== 4'b1111)
      $fatal(1, "forced_held was %b, expected 1111", forced_held);
    if (direct_under_force !== 4'b0100)
      $fatal(1, "direct_under_force was %b, expected 0100",
             direct_under_force);
    if (forced_released !== 4'b0100)
      $fatal(1, "forced_released was %b, expected 0100", forced_released);
    if (chain_forced !== 4'b1010)
      $fatal(1, "chain_forced was %b, expected 1010", chain_forced);
    if (chain_held !== 4'b1010)
      $fatal(1, "chain_held was %b, expected 1010", chain_held);
    if (chain_released !== 4'b0111)
      $fatal(1, "chain_released was %b, expected 0111", chain_released);
    if (typed_first !== 4'b0001)
      $fatal(1, "typed_first was %b, expected 0001", typed_first);
    if (typed_forced !== 4'b1100)
      $fatal(1, "typed_forced was %b, expected 1100", typed_forced);
    if (typed_released !== 4'b0111)
      $fatal(1, "typed_released was %b, expected 0111", typed_released);
    if (source_forced !== 4'b0101)
      $fatal(1, "source_forced was %b, expected 0101", source_forced);
    if (net_moved !== 4'b1001)
      $fatal(1, "net_moved was %b, expected 1001", net_moved);
    $display("All checks passed");
  end
endmodule
