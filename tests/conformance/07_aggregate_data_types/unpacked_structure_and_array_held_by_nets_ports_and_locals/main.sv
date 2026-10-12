// An unpacked structure and a fixed-size unpacked array are held by everything
// that can hold a value (LRM 7.2, 7.4): a variable, a net whose one driver is
// the whole of a variable, an input port declared as a variable and one
// declared as a net (LRM 23.3.3), a procedural continuous assignment and its
// release (LRM 10.6.2), a local of a task that waits between its writes and
// reads (LRM 13.4.2), a local a forked process declares from one of its
// parent's (LRM 9.3.2), and a `ref` port whose sampled value is read (LRM
// 16.5.1). A structure with a string member and one of several thousand bits
// are among them. Each shows the value its source gave it, and a wait on a
// member of a port wakes once for each value that moved that member, which a
// change made under a force does not.
typedef struct {
  logic [7:0] tag;
  logic [99:0] wide;
  logic [3:0] lanes[3];
} record_t;

typedef logic [15:0] row_t[4];

typedef struct {
  logic [8191:0] bulk;
  int count;
} oversized_t;

typedef struct {
  string name;
  int count;
} named_t;

module Leaf (input record_t in);
  int changes = 0;
  always @(in.tag) changes++;
endmodule

module NetLeaf (input wire record_t in);
  int changes = 0;
  always @(in.tag) changes++;
endmodule

module RowLeaf (input wire row_t in);
  int changes = 0;
  always @(in[0]) changes++;
endmodule

module Sampler (ref named_t r, ref row_t row, input logic clk);
  named_t seen;
  row_t seen_row;
  always @(posedge clk) begin
    seen = $sampled(r);
    seen_row = $sampled(row);
  end
endmodule

module Top;
  localparam record_t A = '{tag: 8'h5A, wide: {4'hA, 96'h5}, lanes: '{1, 2, 3}};
  localparam record_t B = '{tag: 8'hC3, wide: {4'h3, 96'hC}, lanes: '{4, 5, 6}};
  localparam record_t C = '{tag: 8'h96, wide: {4'h6, 96'h9}, lanes: '{7, 8, 9}};
  localparam row_t RA = '{16'h1111, 16'h2222, 16'h3333, 16'h4444};
  localparam row_t RB = '{16'hAAAA, 16'hBBBB, 16'hCCCC, 16'hDDDD};

  record_t v;
  row_t row;
  wire record_t shown;
  wire row_t row_shown;
  assign shown = v;
  assign row_shown = row;

  Leaf typed (.in(v));
  NetLeaf through_net (.in(shown));
  RowLeaf row_net (.in(row_shown));

  logic clk = 0;
  named_t named = '{name: "first", count: 1};
  Sampler sampler (.r(named), .row(row), .clk(clk));

  oversized_t oversized;
  record_t early_shown, forced_shown, released_shown;
  row_t early_row, forced_row, released_row;
  named_t held_across_wait, captured;
  int typed_before, net_before, row_before;

  task automatic hold_across_wait(input named_t given, output named_t out);
    named_t waited;
    waited = given;
    #1;
    out = waited;
  endtask

  initial begin
    automatic named_t kept;
    v = A;
    row = RA;
    oversized.bulk = {4'hF, 8184'h0, 4'h1};
    oversized.count = 7;
    #1;
    early_shown = shown;
    early_row = row_shown;
    typed_before = typed.changes;
    net_before = through_net.changes;
    row_before = row_net.changes;

    clk = 1;
    named = '{name: "second", count: 2};
    row = RB;
    #1;

    force shown = B;
    force row_shown = RB;
    #1;
    forced_shown = shown;
    forced_row = row_shown;
    v = C;
    row = RA;
    #1;
    release shown;
    release row_shown;
    #1;
    released_shown = shown;
    released_row = row_shown;

    hold_across_wait('{name: "waited", count: 3}, held_across_wait);

    kept = '{name: "kept", count: 4};
    fork
      automatic named_t copy = kept;
      #1 captured = copy;
    join_none
    kept = '{name: "changed", count: 5};
  end

  final begin
    if (early_shown !== A || early_row !== RA)
      $fatal(1, "before any force the nets showed %p and %p", early_shown,
             early_row);
    if (forced_shown !== B || forced_row !== RB)
      $fatal(1, "under a force the nets showed %p and %p", forced_shown,
             forced_row);
    if (released_shown !== C || released_row !== RA)
      $fatal(1, "once released the nets showed %p and %p", released_shown,
             released_row);
    if (typed.in !== C || through_net.in !== C)
      $fatal(1, "the ports read %p and %p, expected the variable's", typed.in,
             through_net.in);
    if (typed.changes - typed_before != 1)
      $fatal(1, "a variable port woke %0d times for one change",
             typed.changes - typed_before);
    if (through_net.changes - net_before != 2)
      $fatal(1, "a net port woke %0d times for a force and a release",
             through_net.changes - net_before);
    if (row_net.changes - row_before != 2)
      $fatal(1, "an array net port woke %0d times for a change and a release",
             row_net.changes - row_before);
    if (sampler.seen.name != "first" || sampler.seen.count != 1)
      $fatal(1, "a sampled structure read through a ref was %p", sampler.seen);
    if (sampler.seen_row !== RA)
      $fatal(1, "a sampled array read through a ref was %p",
             sampler.seen_row);
    if (oversized.bulk !== {4'hF, 8184'h0, 4'h1} || oversized.count != 7)
      $fatal(1, "an oversized structure held %h... and %0d",
             oversized.bulk[8191 -: 8], oversized.count);
    if (held_across_wait.name != "waited" || held_across_wait.count != 3)
      $fatal(1, "a local read after a wait was %p", held_across_wait);
    if (captured.name != "kept" || captured.count != 4)
      $fatal(1, "a captured local read %p, expected the value captured",
             captured);
    $display("All checks passed");
  end
endmodule
