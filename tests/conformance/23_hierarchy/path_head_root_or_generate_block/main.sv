// The head of a hierarchical path need not be a module instance. $root names
// the root of the instantiation tree, which makes the path that follows it
// absolute and independent of where the reference is written (LRM 23.3.1,
// 23.6); and the head of an upward reference may be a named generate block
// found by searching outward from the reference, since a generate block name
// is one of the forms a head may take (LRM 23.8). Either head reaches an
// object declared on the head itself and carries on past it into an instance
// below, and either may be written through as well as read.
module Leaf;
  int from_root;
  int on_block;
  int past_block;
  int through_block;

  always_comb from_root = $root.Top.g;
  always_comb on_block = blk.bg;
  always_comb past_block = blk.m.ms;
  always_comb through_block = $root.Top.blk.bg;

  initial begin
    #3;
    blk.bg = 9;
  end
endmodule

module Mid;
  int ms;
  Leaf l();

  initial ms = 3;
endmodule

module Top;
  int g;
  int snap_from_root;
  int snap_on_block;
  int snap_past_block;
  int snap_through_block;

  if (1) begin : blk
    int bg;
    Mid m();
    initial bg = 5;
  end

  initial begin
    g = 7;
    #1;
    snap_from_root = blk.m.l.from_root;
    snap_on_block = blk.m.l.on_block;
    snap_past_block = blk.m.l.past_block;
    snap_through_block = blk.m.l.through_block;
  end

  final begin
    if (snap_from_root !== 7)
      $fatal(1, "snap_from_root was %0d, expected 7", snap_from_root);
    if (snap_on_block !== 5)
      $fatal(1, "snap_on_block was %0d, expected 5", snap_on_block);
    if (snap_past_block !== 3)
      $fatal(1, "snap_past_block was %0d, expected 3", snap_past_block);
    if (snap_through_block !== 5)
      $fatal(1, "snap_through_block was %0d, expected 5", snap_through_block);
    if (blk.bg !== 9) $fatal(1, "blk.bg was %0d, expected 9", blk.bg);
    $display("All checks passed");
  end
endmodule
