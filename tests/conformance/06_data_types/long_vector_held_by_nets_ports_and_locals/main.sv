// A vector far longer than a word is held by everything that can hold a value
// (LRM 6.9.1): a net one driver decides, a net whose one driver is the whole of
// a variable, an input port declared as a variable and one declared as a net
// (LRM 23.3.3), a procedural continuous assignment and its release (LRM
// 10.6.2), a local of a task that waits between its writes and reads (LRM
// 13.4.2), and a local a forked process declares from one of its parent's
// (LRM 9.3.2). Each shows the value its source gave it, every bit of it, and a
// wait on a port wakes once for each value that moved what it shows, which a
// change made under a force does not.
module Leaf (input logic [8191:0] in);
  int changes = 0;
  always @(in) changes++;
endmodule

module NetLeaf (input wire [8191:0] in);
  int changes = 0;
  always @(in) changes++;
endmodule

module Top;
  localparam int W = 8192;
  typedef logic [W-1:0] vector;

  localparam vector A = {4'hA, {(W - 8){1'b0}}, 4'h5};
  localparam vector B = {4'h3, {(W - 8){1'b1}}, 4'hC};
  localparam vector C = {4'h6, {(W - 8){1'b0}}, 4'h9};

  vector v;
  wire vector shown;
  wire vector inverted;
  assign shown = v;
  assign inverted = ~v;

  Leaf typed (.in(v));
  NetLeaf through_net (.in(shown));

  vector early_shown, forced_shown, released_shown;
  vector early_inverted, forced_inverted, released_inverted;
  vector held_across_wait, captured;
  int typed_before, net_before;

  task automatic hold_across_wait(input vector given, output vector out);
    vector waited;
    waited = given;
    #1;
    out = waited;
  endtask

  initial begin
    automatic vector kept;
    v = A;
    #1;
    early_shown = shown;
    early_inverted = inverted;
    typed_before = typed.changes;
    net_before = through_net.changes;

    force shown = B;
    force inverted = B;
    #1;
    forced_shown = shown;
    forced_inverted = inverted;
    v = C;
    #1;
    release shown;
    release inverted;
    #1;
    released_shown = shown;
    released_inverted = inverted;

    hold_across_wait(B, held_across_wait);

    kept = C;
    fork
      automatic vector copy = kept;
      #1 captured = copy;
    join_none
    kept = A;
  end

  final begin
    if (early_shown !== A || early_inverted !== ~A)
      $fatal(1, "before any force the nets showed %h... and %h...",
             early_shown[W-1 -: 8], early_inverted[W-1 -: 8]);
    if (forced_shown !== B || forced_inverted !== B)
      $fatal(1, "under a force the nets showed %h... and %h...",
             forced_shown[W-1 -: 8], forced_inverted[W-1 -: 8]);
    if (released_shown !== C || released_inverted !== ~C)
      $fatal(1, "once released the nets showed %h... and %h...",
             released_shown[W-1 -: 8], released_inverted[W-1 -: 8]);
    if (typed.in !== C || through_net.in !== C)
      $fatal(1, "the ports read %h... and %h..., expected the variable's",
             typed.in[W-1 -: 8], through_net.in[W-1 -: 8]);
    if (typed.changes - typed_before != 1)
      $fatal(1, "a variable port woke %0d times for one change",
             typed.changes - typed_before);
    if (through_net.changes - net_before != 2)
      $fatal(1, "a net port woke %0d times for a force and a release",
             through_net.changes - net_before);
    if (held_across_wait !== B)
      $fatal(1, "a local read after a wait was %h...",
             held_across_wait[W-1 -: 8]);
    if (captured !== C)
      $fatal(1, "a captured local read %h..., expected the value captured",
             captured[W-1 -: 8]);
    $display("All checks passed");
  end
endmodule
