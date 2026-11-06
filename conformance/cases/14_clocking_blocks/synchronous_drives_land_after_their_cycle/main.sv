// A synchronous drive evaluates its value and its target's selects where it is
// reached and does not block (LRM 14.16). It matures at the clocking event its
// cycle delay counts to -- from the event of its own time step, or, made
// between events, from the next one, so that `cb.v <= e` there is
// `##1 cb.v <= e`, and `##0` is no cycle delay at all (LRM 14.11) -- and lands
// in the Re-NBA region after the output's skew: of the event's time step for a
// skew of zero, written or not, that delay later for a delay, and at the
// skew's edge of the clock, or that delay after it, for an edge (LRM 14.3,
// 14.4). The region is Re-NBA whichever region set the driving process runs
// in. An output whose signal is a net drives it through a (strong1, strong0)
// driver of its own, 'z until the first drive (LRM 14.16); of drives landing
// in one Re-NBA pass only the last value each part was given is driven, the
// last made winning whichever cycle each was made in, a bit-select drives only
// its bits, and of two blocks outputting one variable the drive landing last
// sets it (LRM 14.16.2); and a drive leaves what the inout's input half reads
// alone until the next event samples it (LRM 14.16.1).
// A skew is read in the time unit of the scope declaring the block, a fraction
// of it included (LRM 14.4, 3.14): at the rising edge at 5, a `#1.5` output
// lands at 6.5.
module FractionalOutputSkew(output logic [7:0] y_before,
                            output logic [7:0] y_after);
  timeunit 1ns;
  timeprecision 100ps;
  logic c = 0;
  always #5 c = ~c;
  logic [7:0] y = 0;
  clocking k @(posedge c);
    output #1.5 y;
  endclocking
  initial begin
    @(k);         // 5
    k.y <= 8'h07;
    #1.4 y_before = y;
    #0.2 y_after = y;
  end
endmodule

module Top;
  timeunit 1ns;
  timeprecision 100ps;
  logic clk = 0;
  always #10 clk = ~clk;  // rises at 10, 30, 50, ...; falls at 20, 40, ...

  logic [7:0] v = 0;
  wire [7:0] w;
  logic [7:0] s = 0;
  logic [7:0] e = 0;
  logic [7:0] e2 = 0;
  logic [7:0] io = 8'h01;
  logic [7:0] r = 0;
  int i = 0;
  event go;

  clocking cb @(posedge clk);
    output v;
    output w;
    output #2 s;
    output negedge e;
    output negedge #5 e2;
    inout io;
  endclocking
  // A weaker driver the block's own (strong1, strong0) one overrides once it
  // drives (LRM 14.16).
  assign (weak1, weak0) w = 8'hF0;

  // An explicit `#0` skew lands where a skew of none does, in Re-NBA after the
  // block's event, not in an Inactive region before it (LRM 14.4).
  logic [7:0] z = 0;
  clocking cz @(posedge clk);
    output #0 z;
  endclocking
  logic [7:0] z_seen_at_110;
  always @(cz) if ($time == 110) z_seen_at_110 = z;

  // A nonblocking assignment of the same time step lands in NBA, before the
  // drive: a process the block's event wakes sees it, and the drive then
  // overwrites it.
  always @(posedge clk) if ($time == 30) v <= 8'hEE;
  logic [7:0] v_seen_at_30;
  always @(cb) if ($time == 30) v_seen_at_30 = v;

  // Made later in the cycle than the program's drive below, from the active
  // region set, while the program's runs in the reactive set.
  always @(go) cb.s <= 8'hF2;

  // Two drives landing in one time step, the second made after the reactive
  // set ran once, land in two passes of the Re-NBA region, so the output
  // changes twice (LRM 14.16.2).
  logic [7:0] g = 8'h01;
  event gx;
  int g_changes = 0;
  always @(g) g_changes++;
  clocking cg @(posedge clk);
    output g;
  endclocking
  Driver driver();
  logic [7:0] fractional_y_before, fractional_y_after;
  FractionalOutputSkew fractional(fractional_y_before, fractional_y_after);

  // Drives of one output landing in one pass of the Re-NBA region put only
  // the last value each gave a part onto the variable, so a value the pass
  // took back is no change at all, and a part a drive left alone keeps its
  // value (LRM 14.16.2).
  logic [7:0] q = 8'h03;
  logic [7:0] p = 8'h80;
  int q_changes = 0;
  int p_changes = 0;
  always @(q) q_changes++;
  always @(p) p_changes++;
  clocking cq @(posedge clk);
    output q, p;
  endclocking
  logic [7:0] q_at_51 = 0, p_at_51 = 0;
  int q_changes_at_51 = -1, p_changes_at_51 = -1;
  initial begin
    #40 @(cq);              // 50
    cq.q <= 8'h05;
    cq.q <= 8'h03;
    cq.p[3:0] <= 4'hF;
    cq.p[1:0] <= 2'b00;
    cq.p[6] <= 1'b1;
    #1 q_at_51 = q;
    p_at_51 = p;
    q_changes_at_51 = q_changes;
    p_changes_at_51 = p_changes;
  end

  // Two blocks on different edges output one variable, which takes the value
  // the drive landing last gave it, not the value of the drive made last (LRM
  // 14.16.2).
  logic [7:0] j = 0;
  clocking jp @(posedge clk);
    output j;
  endclocking
  clocking jn @(negedge clk);
    output j;
  endclocking
  logic [7:0] j_at_21 = 0, j_at_71 = 0;

  // Two blocks on one edge outputting one variable, whose drives land in one
  // pass interleaved: the variable takes, part by part, the value of the drive
  // that landed last, whichever block made it (LRM 14.16.2).
  logic [7:0] m = 8'h00;
  logic [7:0] n = 8'hF0;
  clocking ma @(posedge clk);
    output m, n;
  endclocking
  clocking mb @(posedge clk);
    output m, n;
  endclocking
  logic [7:0] m_at_91 = 0, n_at_91 = 0;
  initial begin
    #80 @(ma);              // 90
    ma.m <= 8'h01;
    mb.m <= 8'h00;
    ma.m <= 8'h01;
    ma.n[0] <= 1'b1;
    mb.n[0] <= 1'b0;
    ma.n[1] <= 1'b1;
    #1 m_at_91 = m;
    n_at_91 = n;
  end

  initial begin
    @(jp);                  // 10
    jp.j <= ##3 8'h01;      // made first, lands at 70
    jn.j <= ##1 8'h02;      // made after it, lands at 20
    #11 j_at_21 = j;        // 21
    #50 j_at_71 = j;        // 71
  end

  logic [7:0] w_before, v_at_11, w_at_11, s_at_11, s_at_13, e_at_13, e_at_23;
  logic [7:0] v_at_29, v_at_31, w_at_31, v_at_49, v_at_51, v_at_71, v_at_91;
  logic [7:0] z_at_111, v_at_131, v_at_171, s_at_173, g_at_191;
  int g_changes_at_191;
  logic [7:0] io_read, io_at_11, io_at_31;
  logic [7:0] e2_at_23, e2_at_29, s_at_91;
  bit done = 0;

  initial begin
    #1 w_before = w;
    @(cb);                  // 10
    cb.v <= 8'h01;
    cb.w <= 8'h11;
    cb.s <= 8'h03;
    cb.e <= 8'h04;
    cb.e2 <= 8'h44;         // lands 5 after the falling edge at 20
    cb.io <= 8'h02;
    #1 v_at_11 = v;         // 11
    w_at_11 = w;
    s_at_11 = s;
    io_read = cb.io;
    io_at_11 = io;
    #2 s_at_13 = s;         // 13
    e_at_13 = e;
    #10 e_at_23 = e;        // 23
    e2_at_23 = e2;
    cb.v <= 8'h05;          // made between events: lands at 30
    cb.v <= ##2 8'h06;      // the second event after it: lands at 50
    r = 8'h22;
    cb.w <= ##1 r;          // lands at 30, with the value r had here
    r = 8'h33;
    #6 v_at_29 = v;         // 29
    e2_at_29 = e2;
    #2 v_at_31 = v;         // 31
    w_at_31 = w;
    io_at_31 = cb.io;
    #18 v_at_49 = v;        // 49
    #2 v_at_51 = v;         // 51
    cb.v <= 8'h07;          // both land at 70; the last made wins
    cb.v <= 8'h08;
    cb.s <= ##0 8'h0C;      // ##0 is no cycle delay: lands at 72
    #20 v_at_71 = v;        // 71
    i = 1;
    cb.v[i*4 +: 4] <= 4'hA; // lands at 90, in bits 7:4 only
    i = 0;
    #20 v_at_91 = v;        // 91
    s_at_91 = s;
    cz.z <= 8'hD1;          // lands at 110, after the block's event
    #20 z_at_111 = z;       // 111
    @(posedge clk);         // 130, before the block's event
    cb.v <= 8'h0B;          // its time step has had its event: lands at 130
    cb.v <= ##2 8'hC1;      // matures at 170
    #1 v_at_131 = v;        // 131
    @(posedge clk);         // 150
    cb.v <= ##1 8'hC2;      // also matures at 170, and was made later
    #21 v_at_171 = v;       // 171
    #2 s_at_173 = s;        // 173
    @(cg);                  // 190
    cg.g <= 8'h00;
    @(gx);                  // the program's, still at 190
    cg.g <= 8'h01;
    #1 g_at_191 = g;        // 191
    g_changes_at_191 = g_changes;
    done = 1;
    $finish;
  end

  final begin
    if (!done) $fatal(1, "the drives were not all made");
    if (w_before !== 8'hF0) $fatal(1, "the net before any drive was %h, expected f0", w_before);
    if (v_at_11 !== 8'h01) $fatal(1, "v at 11 was %h, expected 01", v_at_11);
    if (w_at_11 !== 8'h11) $fatal(1, "w at 11 was %h, expected 11", w_at_11);
    if (s_at_11 !== 8'h00) $fatal(1, "s at 11 was %h, expected 00", s_at_11);
    if (io_at_11 !== 8'h02) $fatal(1, "the inout's signal at 11 was %h, expected 02", io_at_11);
    if (io_read !== 8'h01) $fatal(1, "the inout read %h after its drive landed, expected 01", io_read);
    if (s_at_13 !== 8'h03) $fatal(1, "s at 13 was %h, expected 03", s_at_13);
    if (e_at_13 !== 8'h00) $fatal(1, "e at 13 was %h, expected 00", e_at_13);
    if (e_at_23 !== 8'h04) $fatal(1, "e at 23 was %h, expected 04", e_at_23);
    if (e2_at_23 !== 8'h00) $fatal(1, "e2 at 23 was %h, expected 00", e2_at_23);
    if (e2_at_29 !== 8'h44) $fatal(1, "e2 at 29 was %h, expected 44", e2_at_29);
    if (v_at_29 !== 8'h01) $fatal(1, "v at 29 was %h, expected 01", v_at_29);
    if (v_seen_at_30 !== 8'hEE) $fatal(1, "v at the event at 30 was %h, expected ee", v_seen_at_30);
    if (v_at_31 !== 8'h05) $fatal(1, "v at 31 was %h, expected 05", v_at_31);
    if (w_at_31 !== 8'h22) $fatal(1, "w at 31 was %h, expected 22", w_at_31);
    if (io_at_31 !== 8'h02) $fatal(1, "the inout read %h at 31, expected 02", io_at_31);
    if (v_at_49 !== 8'h05) $fatal(1, "v at 49 was %h, expected 05", v_at_49);
    if (v_at_51 !== 8'h06) $fatal(1, "v at 51 was %h, expected 06", v_at_51);
    if (v_at_71 !== 8'h08) $fatal(1, "v at 71 was %h, expected 08", v_at_71);
    if (v_at_91 !== 8'hA8) $fatal(1, "v at 91 was %h, expected a8", v_at_91);
    if (s_at_91 !== 8'h0C) $fatal(1, "s at 91 was %h, expected 0c", s_at_91);
    if (z_seen_at_110 !== 8'h00) $fatal(1, "z at the event at 110 was %h, expected 00", z_seen_at_110);
    if (z_at_111 !== 8'hD1) $fatal(1, "z at 111 was %h, expected d1", z_at_111);
    if (v_at_131 !== 8'h0B) $fatal(1, "v at 131 was %h, expected 0b", v_at_131);
    if (v_at_171 !== 8'hC2) $fatal(1, "v at 171 was %h, expected c2", v_at_171);
    if (s_at_173 !== 8'hF2) $fatal(1, "s at 173 was %h, expected f2", s_at_173);
    if (g_at_191 !== 8'h01) $fatal(1, "g at 191 was %h, expected 01", g_at_191);
    if (g_changes_at_191 !== 2) $fatal(1, "g changed %0d times at 190, expected 2", g_changes_at_191);
    if (q_at_51 !== 8'h03) $fatal(1, "q at 51 was %h, expected 03", q_at_51);
    if (q_changes_at_51 !== 0) $fatal(1, "q changed %0d times at 50, expected 0", q_changes_at_51);
    if (p_at_51 !== 8'hCC) $fatal(1, "p at 51 was %h, expected cc", p_at_51);
    if (p_changes_at_51 !== 1) $fatal(1, "p changed %0d times at 50, expected 1", p_changes_at_51);
    if (j_at_21 !== 8'h02) $fatal(1, "j at 21 was %h, expected 02", j_at_21);
    if (j_at_71 !== 8'h01) $fatal(1, "j at 71 was %h, expected 01", j_at_71);
    if (m_at_91 !== 8'h01) $fatal(1, "m at 91 was %h, expected 01", m_at_91);
    if (n_at_91 !== 8'hF2) $fatal(1, "n at 91 was %h, expected f2", n_at_91);
    if (fractional_y_before !== 8'h00) $fatal(1, "a #1.5 output landed early: %h", fractional_y_before);
    if (fractional_y_after !== 8'h07) $fatal(1, "a #1.5 output left %h, expected 07", fractional_y_after);
    $display("All checks passed");
  end
endmodule

// Drives an output from the reactive region set (LRM 24.3.1), earlier in the
// cycle than the module's drive it wakes, and later wakes a second drive in the
// time step of a first one.
program Driver;
  timeunit 1ns;
  timeprecision 100ps;
  initial begin
    #165 @(Top.cb);         // 170
    Top.cb.s <= 8'hF1;      // lands at 172, before the module's
    -> Top.go;
    @(Top.cg);              // 190
    -> Top.gx;
    #5;
  end
endprogram
