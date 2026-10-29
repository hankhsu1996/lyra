// An input port declared with no data type is a net (LRM 23.2.2.3), driven by
// the continuous assignment its connection implies (LRM 23.3.3). Connected to
// the whole of a variable or of a net of its own type, it carries that object's
// value, every change of which is a change of the port. Whether its first
// value at time zero reaches a procedure already waiting on it depends on an
// order the standard leaves open (LRM 4.7), so changes are counted from after
// time zero. Handed on to another such port, the value
// is the same at every level, and so it is where the port in between is
// declared as a variable (LRM 23.3.3.2). A force on the port overrides the
// port and whatever it is handed on to, never what drives it, and its release
// shows what drives it again, with whatever that moved to meanwhile; a force
// on the source reaches the port as the assignment carries it (LRM 10.6.2). A
// connection to a part of a variable carries that part. A value wider than a
// word is carried as a narrow one is.
//
// A force and its release are events of the port only where its value moves,
// and never events of what drives it (LRM 10.6.2, 4.3). A second force on a
// net replaces the first, and of two ports forced one inside the other, each
// shows its own forced value until released, and then what drives it then.
// A force begun and ended before the port's driver has given it any value, or
// still standing when it does, leaves the port showing what drives it once
// released, as a net with one driver that carries only an expression does.
module Leaf (
    input [3:0] in
);
  int changes;
  int star;
  logic [3:0] star_seen;
  always @(in) changes++;
  always @* begin
    star_seen = in;
    star++;
  end
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

module WideLeaf (
    input [99:0] in
);
  int changes;
  always @(in) changes++;
endmodule

module TypedWideMid (
    input logic [99:0] in
);
  WideLeaf leaf (.in(in));
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

  // A value wider than a word, from each kind of source.
  logic [99:0] wv = {4'hA, 96'h1};
  wire [99:0] wn;
  assign wn = wv;
  WideLeaf wide_direct (.in(wv));
  WideLeaf wide_from_net (.in(wn));
  TypedWideMid wide_typed (.in(wv));

  logic [3:0] s = 4'b0010;
  int s_star;
  logic [3:0] s_seen;
  Leaf watched (.in(s));
  always @* begin
    s_seen = s;
    s_star++;
  end

  logic [3:0] t = 4'b0001;
  Mid outer_first (.in(t));
  Mid inner_first (.in(t));

  logic [3:0] e = 4'b0100;
  logic [3:0] h = 4'b1000;
  wire [3:0] by_expr_early, by_expr_held;
  assign by_expr_early = ~e;
  assign by_expr_held = ~h;
  Leaf early (.in(e));
  Leaf early_held (.in(h));
  event held_ends;
  always begin
    force early.in = 4'b1110;
    release early.in;
    force by_expr_early = 4'b0000;
    release by_expr_early;
    force early_held.in = 4'b1101;
    force by_expr_held = 4'b0101;
    @held_ends;
    release early_held.in;
    release by_expr_held;
    @held_ends;
  end

  logic [3:0] typed_first, typed_forced, typed_released;
  logic [3:0] direct_first, chain_first, net_first;
  int changes_first;
  logic [3:0] direct_moved, chain_moved;
  int changes_moved, changes_same;
  logic [3:0] forced_seen, source_under_force, forced_held, direct_under_force;
  logic [3:0] forced_released;
  logic [3:0] chain_forced, chain_held, chain_released;
  logic [3:0] source_forced;
  logic [3:0] net_moved, member_seen;

  int s_star_before, s_star_after;
  int watched_star_before, watched_changes_before;
  int watched_star_moved, watched_changes_moved;
  int watched_star_same, watched_changes_same;
  logic [3:0] watched_released, watched_star_seen;

  initial begin
    #1;
    s_star_before = s_star;
    watched_star_before = watched.star;
    watched_changes_before = watched.changes;
    force watched.in = 4'b1000;
    #1;
    release watched.in;
    #1;
    s_star_after = s_star;
    watched_star_moved = watched.star;
    watched_changes_moved = watched.changes;
    watched_released = watched.in;
    watched_star_seen = watched.star_seen;

    // Forced and released at the value the port already shows: neither is a
    // change of the port.
    force watched.in = 4'b0010;
    #1;
    release watched.in;
    #1;
    watched_star_same = watched.star;
    watched_changes_same = watched.changes;
  end

  logic [3:0] outer_replaced, outer_both, outer_inner_left, outer_inner_held;
  logic [3:0] outer_inner_released, outer_inner_follows;
  logic [3:0] inner_both, inner_shows_outer, inner_released;

  initial begin
    #1;
    force outer_first.in = 4'b1111;
    #1;
    force outer_first.in = 4'b1010;
    #1;
    outer_replaced = outer_first.leaf.in;
    force outer_first.leaf.in = 4'b0101;
    t = 4'b0011;
    #1;
    outer_both = outer_first.leaf.in;
    release outer_first.in;
    #1;
    outer_inner_left = outer_first.in;
    outer_inner_held = outer_first.leaf.in;
    t = 4'b0110;
    #1;
    release outer_first.leaf.in;
    #1;
    outer_inner_released = outer_first.leaf.in;
    t = 4'b1110;
    #1;
    outer_inner_follows = outer_first.leaf.in;

    force inner_first.in = 4'b1001;
    force inner_first.leaf.in = 4'b0110;
    #1;
    inner_both = inner_first.leaf.in;
    release inner_first.leaf.in;
    #1;
    inner_shows_outer = inner_first.leaf.in;
    release inner_first.in;
    #1;
    inner_released = inner_first.leaf.in;
  end

  logic [99:0] wide_direct_first, wide_net_first, wide_typed_first;
  logic [99:0] wide_typed_moved, wide_typed_forced, wide_typed_held;
  logic [99:0] wide_typed_released;
  int wide_typed_changes_before, wide_typed_changes_after;

  initial begin
    #1;
    wide_direct_first = wide_direct.in;
    wide_net_first = wide_from_net.in;
    wide_typed_first = wide_typed.leaf.in;
    wide_typed_changes_before = wide_typed.leaf.changes;
    wv = {4'h5, 96'h2};
    #1;
    wide_typed_moved = wide_typed.leaf.in;
    force wide_typed.leaf.in = {100{1'b1}};
    #1;
    wide_typed_forced = wide_typed.leaf.in;
    wv = {4'h3, 96'h4};
    #1;
    wide_typed_held = wide_typed.leaf.in;
    release wide_typed.leaf.in;
    #1;
    wide_typed_released = wide_typed.leaf.in;
    wide_typed_changes_after = wide_typed.leaf.changes;
  end

  logic [3:0] early_first, early_follows, expr_early_first, expr_early_follows;
  logic [3:0] held_first, held_released, held_follows;
  logic [3:0] expr_held_first, expr_held_released;

  initial begin
    #1;
    early_first = early.in;
    expr_early_first = by_expr_early;
    held_first = early_held.in;
    expr_held_first = by_expr_held;
    ->held_ends;
    #1;
    held_released = early_held.in;
    expr_held_released = by_expr_held;
    e = 4'b0011;
    h = 4'b0111;
    #1;
    early_follows = early.in;
    expr_early_follows = by_expr_early;
    held_follows = early_held.in;
  end

  initial begin
    b = 4'b0110;
    r = '{tag: 4'h9, payload: 4'h5};
    #1;
    direct_first = direct.in;
    chain_first = chain.leaf.in;
    net_first = from_net.in;
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
    if (member_seen !== 4'h9)
      $fatal(1, "member_seen was %h, expected 9", member_seen);
    if (direct_moved !== 4'b0011)
      $fatal(1, "direct_moved was %b, expected 0011", direct_moved);
    if (chain_moved !== 4'b0011)
      $fatal(1, "chain_moved was %b, expected 0011", chain_moved);
    if (changes_moved !== changes_first + 1)
      $fatal(1, "direct.changes moved from %0d to %0d, expected one more",
             changes_first, changes_moved);
    if (changes_same !== changes_moved)
      $fatal(1, "direct.changes moved from %0d to %0d on an equal value",
             changes_moved, changes_same);
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

    if (s_star_after !== s_star_before)
      $fatal(1, "s_star moved from %0d to %0d across a force of the port",
             s_star_before, s_star_after);
    if (watched_star_moved !== watched_star_before + 2)
      $fatal(1, "watched.star moved from %0d to %0d, expected two more",
             watched_star_before, watched_star_moved);
    if (watched_changes_moved !== watched_changes_before + 2)
      $fatal(1, "watched.changes moved from %0d to %0d, expected two more",
             watched_changes_before, watched_changes_moved);
    if (watched_released !== 4'b0010)
      $fatal(1, "watched_released was %b, expected 0010", watched_released);
    if (watched_star_seen !== 4'b0010)
      $fatal(1, "watched_star_seen was %b, expected 0010", watched_star_seen);
    if (watched_star_same !== watched_star_moved)
      $fatal(1, "watched.star moved from %0d to %0d under an equal force",
             watched_star_moved, watched_star_same);
    if (watched_changes_same !== watched_changes_moved)
      $fatal(1, "watched.changes moved from %0d to %0d under an equal force",
             watched_changes_moved, watched_changes_same);

    if (outer_replaced !== 4'b1010)
      $fatal(1, "outer_replaced was %b, expected 1010", outer_replaced);
    if (outer_both !== 4'b0101)
      $fatal(1, "outer_both was %b, expected 0101", outer_both);
    if (outer_inner_left !== 4'b0011)
      $fatal(1, "outer_inner_left was %b, expected 0011", outer_inner_left);
    if (outer_inner_held !== 4'b0101)
      $fatal(1, "outer_inner_held was %b, expected 0101", outer_inner_held);
    if (outer_inner_released !== 4'b0110)
      $fatal(1, "outer_inner_released was %b, expected 0110",
             outer_inner_released);
    if (outer_inner_follows !== 4'b1110)
      $fatal(1, "outer_inner_follows was %b, expected 1110",
             outer_inner_follows);
    if (inner_both !== 4'b0110)
      $fatal(1, "inner_both was %b, expected 0110", inner_both);
    if (inner_shows_outer !== 4'b1001)
      $fatal(1, "inner_shows_outer was %b, expected 1001", inner_shows_outer);
    if (inner_released !== 4'b1110)
      $fatal(1, "inner_released was %b, expected 1110", inner_released);

    if (wide_direct_first !== {4'hA, 96'h1})
      $fatal(1, "wide_direct_first was %h", wide_direct_first);
    if (wide_net_first !== {4'hA, 96'h1})
      $fatal(1, "wide_net_first was %h", wide_net_first);
    if (wide_typed_first !== {4'hA, 96'h1})
      $fatal(1, "wide_typed_first was %h", wide_typed_first);
    if (wide_typed_moved !== {4'h5, 96'h2})
      $fatal(1, "wide_typed_moved was %h", wide_typed_moved);
    if (wide_typed_forced !== {100{1'b1}})
      $fatal(1, "wide_typed_forced was %h", wide_typed_forced);
    if (wide_typed_held !== {100{1'b1}})
      $fatal(1, "wide_typed_held was %h", wide_typed_held);
    if (wide_typed_released !== {4'h3, 96'h4})
      $fatal(1, "wide_typed_released was %h", wide_typed_released);
    // Moved once, forced once and released once onto what it then shows.
    if (wide_typed_changes_after !== wide_typed_changes_before + 3)
      $fatal(1, "wide_typed.leaf.changes moved from %0d to %0d, expected 3 more",
             wide_typed_changes_before, wide_typed_changes_after);

    if (early_first !== 4'b0100)
      $fatal(1, "early_first was %b, expected 0100", early_first);
    if (expr_early_first !== 4'b1011)
      $fatal(1, "expr_early_first was %b, expected 1011", expr_early_first);
    if (held_first !== 4'b1101)
      $fatal(1, "held_first was %b, expected 1101", held_first);
    if (expr_held_first !== 4'b0101)
      $fatal(1, "expr_held_first was %b, expected 0101", expr_held_first);
    if (held_released !== 4'b1000)
      $fatal(1, "held_released was %b, expected 1000", held_released);
    if (expr_held_released !== 4'b0111)
      $fatal(1, "expr_held_released was %b, expected 0111",
             expr_held_released);
    if (early_follows !== 4'b0011)
      $fatal(1, "early_follows was %b, expected 0011", early_follows);
    if (expr_early_follows !== 4'b1100)
      $fatal(1, "expr_early_follows was %b, expected 1100",
             expr_early_follows);
    if (held_follows !== 4'b0111)
      $fatal(1, "held_follows was %b, expected 0111", held_follows);
    $display("All checks passed");
  end
endmodule
