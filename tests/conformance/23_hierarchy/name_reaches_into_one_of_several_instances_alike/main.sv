// Every instance of a module is a copy of its own, holding its own variables
// and its own instances of what it instantiates, however many instances are
// written alike (LRM 23.3). A hierarchical name reaches one of them by its
// path and reads that one's variable (LRM 23.6).
module Leaf (
    input int d,
    output int q
);
  int held = -1;

  always_comb held = d + 1;
  assign q = held;
endmodule

module Mid (
    input int d,
    output int q
);
  int between;

  Leaf first (
      .d(d),
      .q(between)
  );
  Leaf second (
      .d(between),
      .q(q)
  );
endmodule

module Top;
  int from[4];
  int out[4];

  for (genvar i = 0; i < 4; i += 1) begin : g
    Mid m (
        .d(from[i]),
        .q(out[i])
    );
  end

  initial begin
    from[0] = 10;
    from[1] = 20;
    from[2] = 30;
    from[3] = 40;
  end

  final begin
    if (out[2] !== 32) $fatal(1, "out[2] was %0d, expected 32", out[2]);
    if (g[2].m.first.held !== 31)
      $fatal(1, "g[2].m.first.held was %0d, expected 31", g[2].m.first.held);
    if (g[3].m.second.held !== 42)
      $fatal(1, "g[3].m.second.held was %0d, expected 42", g[3].m.second.held);
    if (g[1].m.between !== 21)
      $fatal(1, "g[1].m.between was %0d, expected 21", g[1].m.between);
    $display("All checks passed");
  end
endmodule
