// A net has a sampled value as a variable does: its value in the Preponed region
// of the time step, and at time zero its default sampled value, which for a net
// is the default of its type (LRM 16.5.1, 6.8 Table 6-7) -- `x` for a 4-state
// one, whatever its drivers give it at time zero.
//
// What a net shows comes from different places: the one object its one driver
// carries whole, what one driver computes, what several drivers resolve to, a
// force covering it (LRM 10.6.2), and a port with no data type, which is a net
// (LRM 23.2.2.3). Each net here takes `d`, which a procedure writes on the same
// edge another reads the nets' sampled values, so a plain read would be a race;
// the sampled value is the value from before the edge whichever order the two
// procedures took.
typedef struct {
  logic a;
  logic b;
} pair_t;

module Watch (
    input clk,
    input s,
    input wire t
);
  int edges = 0;
  logic seen_s[4];
  logic seen_t[4];

  always @(posedge clk) begin
    edges = edges + 1;
    if (edges <= 4) begin
      seen_s[edges-1] = $sampled(s);
      seen_t[edges-1] = $sampled(t);
    end
  end

  assert property (@(posedge clk) s == t)
  else $fatal(1, "the sampled values of two ports of one source differ");
endmodule

module Top;
  logic clk = 0;
  logic d = 0;

  wire shown;
  wire inverted;
  wire both;
  wire [99:0] wide;
  wire pair_t pair;
  wire held;

  assign shown = d;
  assign inverted = ~d;
  assign both = d;
  assign both = d;
  assign wide = {100{d}};
  assign pair = '{d, ~d};
  assign held = d;

  Watch watch (
      .clk(clk),
      .s  (d),
      .t  (d)
  );

  always #5 clk = ~clk;

  // `d` is 0 until the first edge, at 5, and 1 from then on, so the edge at 5
  // samples 0 and every later one samples 1.
  always @(posedge clk) d = 1;

  // `held` is forced to 0 at the edge at 15 and released at 25, in the same
  // steps the sampled values are read: the edge at 15 samples the 1 from before
  // the force, the edge at 25 the forced 0, and the edge at 35 the 1 `d` gives
  // it again once released.
  always @(posedge clk)
    if ($time == 15) force held = 0;
    else if ($time == 25) release held;

  logic at_zero_shown;
  logic at_zero_inverted;
  logic [99:0] at_zero_wide;

  initial begin
    at_zero_shown = $sampled(shown);
    at_zero_inverted = $sampled(inverted);
    at_zero_wide = $sampled(wide);
  end

  int edges = 0;
  logic seen_shown[4];
  logic seen_inverted[4];
  logic seen_both[4];
  logic [99:0] seen_wide[4];
  pair_t seen_pair[4];
  logic seen_held[4];
  logic rose_at[4];
  logic past_at[4];

  always @(posedge clk) begin
    edges = edges + 1;
    if (edges <= 4) begin
      seen_shown[edges-1] = $sampled(shown);
      seen_inverted[edges-1] = $sampled(inverted);
      seen_both[edges-1] = $sampled(both);
      seen_wide[edges-1] = $sampled(wide);
      seen_pair[edges-1] = $sampled(pair);
      seen_held[edges-1] = $sampled(held);
      rose_at[edges-1] = $rose(shown);
      past_at[edges-1] = $past(inverted);
    end
  end

  initial begin
    #40;
    $finish;
  end

  logic expect_d[4] = '{1'b0, 1'b1, 1'b1, 1'b1};
  logic expect_held[4] = '{1'b0, 1'b1, 1'b0, 1'b1};
  // The first edge has no prior one, so $rose compares with the default
  // sampled value, `x`, and $past answers it (LRM 16.9.3).
  logic expect_rose[4] = '{1'b0, 1'b1, 1'b0, 1'b0};
  logic expect_past[4] = '{1'bx, 1'b1, 1'b0, 1'b0};

  final begin
    if (edges < 4 || watch.edges < 4)
      $fatal(1, "the clock reached %0d edges, expected 4", edges);
    if (at_zero_shown !== 1'bx)
      $fatal(1, "a net's sampled value at time zero was %b, expected x",
             at_zero_shown);
    if (at_zero_inverted !== 1'bx)
      $fatal(1, "a computed net's sampled value at time zero was %b, expected x",
             at_zero_inverted);
    if (at_zero_wide !== {100{1'bx}})
      $fatal(1, "a wide net's sampled value at time zero was not all x");
    for (int e = 0; e < 4; e++) begin
      if (seen_shown[e] !== expect_d[e])
        $fatal(1, "edge %0d: a net showing a variable sampled %b, expected %b",
               e + 1, seen_shown[e], expect_d[e]);
      if (seen_inverted[e] !== ~expect_d[e])
        $fatal(1, "edge %0d: a computed net sampled %b, expected %b", e + 1,
               seen_inverted[e], ~expect_d[e]);
      if (seen_both[e] !== expect_d[e])
        $fatal(1, "edge %0d: a resolved net sampled %b, expected %b", e + 1,
               seen_both[e], expect_d[e]);
      if (seen_wide[e] !== {100{expect_d[e]}})
        $fatal(1, "edge %0d: a wide net sampled the wrong value", e + 1);
      if (seen_pair[e].a !== expect_d[e] || seen_pair[e].b !== ~expect_d[e])
        $fatal(1, "edge %0d: a structure net sampled '{%b, %b}", e + 1,
               seen_pair[e].a, seen_pair[e].b);
      if (seen_held[e] !== expect_held[e])
        $fatal(1, "edge %0d: a forced net sampled %b, expected %b", e + 1,
               seen_held[e], expect_held[e]);
      if (rose_at[e] !== expect_rose[e])
        $fatal(1, "edge %0d: $rose of a net was %b, expected %b", e + 1,
               rose_at[e], expect_rose[e]);
      if (past_at[e] !== expect_past[e])
        $fatal(1, "edge %0d: $past of a net was %b, expected %b", e + 1,
               past_at[e], expect_past[e]);
      if (watch.seen_s[e] !== expect_d[e] || watch.seen_t[e] !== expect_d[e])
        $fatal(1, "edge %0d: ports sampled %b and %b, expected %b", e + 1,
               watch.seen_s[e], watch.seen_t[e], expect_d[e]);
    end
    $display("All checks passed");
  end
endmodule
