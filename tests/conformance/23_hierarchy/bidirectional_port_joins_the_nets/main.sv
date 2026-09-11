// An `inout` port connection is not a directional edge. LRM 23.3.3 makes it a
// non-strength-reducing transistor connection, and LRM 23.3.3.7 settles what
// that means by merging the nets on both sides into one simulated net. So the
// drivers of both sides are contributions to one resolution: they meet at the
// strengths they were driven at, under the net type's own truth table, and
// both names show the result and wake what waits on them. The connection
// carries no direction, composes through a chain of ports, and leaves a net
// alone where the port is not connected at all.
module Pad(inout wire w, input logic en, input logic d);
  logic seen;
  int changes;

  always_comb seen = w;
  always @(w) changes++;

  assign w = en ? d : 1'bz;
endmodule

// A resistive pull, which every ordinary driver outranks (LRM 28.12.1). Merged
// with the parent's net its contribution meets the parent's drivers at pull
// strength; re-driven across the port at the strength a port connection
// carries it would meet them at strong instead, and conflict.
module Puller(inout wire w);
  assign (pull1, pull0) w = 1'b1;
endmodule

// The wired-and fold belongs to the simulated net, so a driver on either side
// of the port resolves under it (LRM 6.6.3).
module WandPad(inout wand w);
  assign w = 1'b0;
endmodule

// A port connected to nothing joins no other net and resolves its own drivers
// alone (LRM 23.3.3.3).
module Lonely(inout wire w);
  logic seen;

  always_comb seen = w;
  assign w = 1'b0;
endmodule

module Leaf(inout wire w, input logic en);
  assign w = en ? 1'b1 : 1'bz;
endmodule

// Two bidirectional ports of one instance connected to one net put both in the
// same resolution, so a driver reached through either is a contribution to it.
module Twice(inout wire p, inout wire q);
  logic from_q;

  assign p = 1'b1;
  always_comb from_q = q;
endmodule

// Only observes whatever net it is joined to, so what it shows is a statement
// about the connection rather than about anything it does itself. Instantiated
// twice below: once per element of an array, where every element connects to
// the same net (LRM 23.3.3.5), and once against a net named by a hierarchical
// path (LRM 23.6), which is joined through the same route every connection
// reaches its far side by.
module Element(inout wire w);
  logic seen;

  always_comb seen = w;
endmodule

module Elsewhere;
  wire held;
endmodule

// Handing a bidirectional port on to a child is the same connection one level
// down, so all three nets are one resolution.
module Mid(inout wire w, input logic en);
  Leaf leaf(.w(w), .en(en));
endmodule

module Top;
  logic child_en;
  logic child_d;
  logic top_en;
  logic top_d;
  wire shared;
  Pad pad(.w(shared), .en(child_en), .d(child_d));
  assign shared = top_en ? top_d : 1'bz;

  wire pulled;
  Puller puller(.w(pulled));
  logic strong_en;
  assign pulled = strong_en ? 1'b0 : 1'bz;

  wand wired;
  WandPad wand_pad(.w(wired));
  assign wired = 1'b1;

  Lonely lonely();

  logic deep_en;
  wire deep;
  Mid mid(.w(deep), .en(deep_en));
  logic deep_top_en;
  assign deep = deep_top_en ? 1'b0 : 1'bz;

  wire twice;
  Twice both(.p(twice), .q(twice));

  wire spread;
  Element elements[2](.w(spread));
  assign spread = 1'b0;

  Elsewhere elsewhere();
  Element named(.w(elsewhere.held));
  assign elsewhere.held = 1'b1;

  logic undriven;
  logic driven_from_child;
  logic driven_from_parent;
  logic driven_from_both;
  logic pull_alone;
  logic deep_from_leaf;
  int changes_before;
  int changes_after;
  logic forced_from_other_name;
  logic released_from_other_name;

  initial begin
    child_en = 1'b0;
    top_en = 1'b0;
    strong_en = 1'b0;
    deep_en = 1'b0;
    deep_top_en = 1'b0;
    #1;
    undriven = shared;
    pull_alone = pulled;
    changes_before = pad.changes;
    deep_en = 1'b1;
    #1;
    deep_from_leaf = deep;
    child_en = 1'b1;
    child_d = 1'b1;
    #1;
    driven_from_child = shared;
    changes_after = pad.changes;
    child_en = 1'b0;
    top_en = 1'b1;
    top_d = 1'b0;
    #1;
    driven_from_parent = shared;
    child_en = 1'b1;
    #1;
    driven_from_both = shared;
    strong_en = 1'b1;
    deep_top_en = 1'b1;
    #1;
    force twice = 1'b0;
    #1;
    forced_from_other_name = both.from_q;
    release twice;
    #1;
    released_from_other_name = both.from_q;
  end

  final begin
    // Neither side driving leaves the merged net at high impedance, and both
    // names show it.
    if (undriven !== 1'bz)
      $fatal(1, "shared undriven was %b, expected z", undriven);
    // A driver inside the child reaches the parent's name, and one in the
    // parent reaches the child's, with no direction stated either way.
    if (driven_from_child !== 1'b1)
      $fatal(1, "shared driven from the child was %b, expected 1",
             driven_from_child);
    if (driven_from_parent !== 1'b0)
      $fatal(1, "shared driven from the parent was %b, expected 0",
             driven_from_parent);
    // Two drivers of opposite value on opposite sides of the port conflict
    // exactly as two drivers on one net do (LRM Table 6-2), and the child's
    // name shows the same conflict the parent's does.
    if (driven_from_both !== 1'bx)
      $fatal(1, "shared driven from both sides was %b, expected x",
             driven_from_both);
    if (pad.seen !== 1'bx)
      $fatal(1, "pad.seen was %b, expected x", pad.seen);
    // A change of the merged net wakes what waits on either of its names. The
    // count is read as a difference across the one interval the net changes
    // in, so what it measures is the wakeup rather than how many times the
    // drivers settled before the design reached a steady state.
    if (changes_after - changes_before !== 1)
      $fatal(1, "pad.changes moved by %0d over one change, expected 1",
             changes_after - changes_before);
    // The pull shows where nothing else drives and is outranked once
    // something does, which is what non-strength-reducing means.
    if (pull_alone !== 1'b1)
      $fatal(1, "pulled with only the pull was %b, expected 1", pull_alone);
    if (pulled !== 1'b0)
      $fatal(1, "pulled against a strong driver was %b, expected 0", pulled);
    // The simulated net's own fold resolves both sides.
    if (wired !== 1'b0)
      $fatal(1, "wired was %b, expected 0 from the wired-and fold", wired);
    if (wand_pad.w !== 1'b0)
      $fatal(1, "wand_pad.w was %b, expected 0", wand_pad.w);
    // A chain of bidirectional ports is one resolution across all three nets,
    // so the name at either end shows what that resolution produced.
    if (deep_from_leaf !== 1'b1)
      $fatal(1, "deep driven from the leaf was %b, expected 1", deep_from_leaf);
    if (deep !== 1'bx)
      $fatal(1, "deep driven from the leaf and the top was %b, expected x",
             deep);
    if (mid.leaf.w !== 1'bx)
      $fatal(1, "mid.leaf.w was %b, expected x", mid.leaf.w);
    // An unconnected bidirectional port is a net of its own.
    if (lonely.seen !== 1'b0)
      $fatal(1, "lonely.seen was %b, expected 0", lonely.seen);
    // Two ports of one instance on one net, an instance array sharing one
    // actual, and an actual named by a hierarchical path each join what they
    // name without needing anything the single connection above did not.
    if (twice !== 1'b1)
      $fatal(1, "twice was %b, expected 1", twice);
    if (both.from_q !== 1'b1)
      $fatal(1, "both.from_q was %b, expected 1", both.from_q);
    if (elements[0].seen !== 1'b0)
      $fatal(1, "elements[0].seen was %b, expected 0", elements[0].seen);
    if (elements[1].seen !== 1'b0)
      $fatal(1, "elements[1].seen was %b, expected 0", elements[1].seen);
    if (named.seen !== 1'b1)
      $fatal(1, "named.seen was %b, expected 1", named.seen);
    // A procedural continuous assignment overrides every driver of the net it
    // names (LRM 10.6.2), and the net it names is the joined one -- so the
    // forced value shows under the other name too, and releasing hands the
    // resolution back to the drivers of both sides.
    if (forced_from_other_name !== 1'b0)
      $fatal(1, "both.from_q under force was %b, expected 0",
             forced_from_other_name);
    if (released_from_other_name !== 1'b1)
      $fatal(1, "both.from_q after release was %b, expected 1",
             released_from_other_name);
    $display("All checks passed");
  end
endmodule
