// A hierarchical name's select is a constant expression evaluated where the
// name is (LRM 23.6), so a name one block of a loop writes with a select
// computed from its own index reaches a different block from every block of
// the loop (LRM 27.4). The loop below counts from 3 by 2, so a block is found
// by the value its index stood at, not by how many came before it. Each block
// reaches the one below it from a process, from a continuous assignment, which
// also wakes on it, by calling a function that block declares, and by writing
// a variable of it. A second loop whose index sizes what each block declares
// makes the same reach where each block is a different shape, from its own
// blocks and from a loop whose index does nothing else. A loop whose blocks
// all name its first block by a constant select reach that one block from
// every block, the first included, whichever block the front end resolved
// the name in. A loop inside a block that stands alone is reached through
// that block, and a class a block declares reaches the block below from its
// method.
module Top;
  localparam int Step = 2;

  for (genvar i = 3; i < 11; i += Step) begin : g
    int t;
    int given = -1;
    int seen = -1;
    int called = -1;
    int driven;

    function automatic int Own();
      return t;
    endfunction

    initial t = i * 10;

    if (i > 3) begin : has_prev
      assign driven = g[i - Step].t + 1;
      initial begin
        #1;
        seen = g[i - Step].t;
        called = g[i - Step].Own();
        g[i - Step].given = i;
      end
    end
  end

  for (genvar k = 1; k < 4; k++) begin : w
    logic [k:0] sized = '1;
    int seen = -1;
    if (k > 1) begin : has_prev
      initial #1 seen = $bits(w[k - 1].sized) * 100 + w[k - 1].sized;
    end
  end

  for (genvar n = 0; n < 4; n++) begin : c
    int t = -1;
    int seen = -1;
    int called = -1;
    int inner_seen = -1;

    function automatic int Own();
      return t;
    endfunction

    if (1) begin : sub
      int u = -1;
    end

    initial begin
      #1;
      seen = c[0].t;
      called = c[0].Own();
      inner_seen = c[0].sub.u;
    end
  end

  if (1) begin : outer
    for (genvar m = 0; m < 2; m++) begin : inner
      int t = m + 40;
    end
  end

  for (genvar p = 0; p < 3; p++) begin : k
    int t = p + 60;
    int seen = -1;
    if (p > 0) begin : has_prev
      class Reader;
        function int Below();
          return k[p - 1].t;
        endfunction
      endclass
      initial begin
        Reader reader = new;
        #1;
        seen = reader.Below();
      end
    end
  end

  int outer_seen = -1;
  initial #1 outer_seen = outer.inner[1].t;

  // A reader whose index nothing else uses reaches a block of a different
  // shape from each of its own.
  for (genvar j = 1; j < 4; j++) begin : r
    int seen = -1;
    initial #1 seen = w[j].sized;
  end

  // A change to a block's variable after time 0 reaches the block above it
  // through the continuous assignment there.
  initial #2 g[7].t = 7;

  initial begin
    c[0].t = 100;
    c[1].t = 1;
    c[2].t = 2;
    c[3].t = 3;
    c[0].sub.u = 200;
    c[1].sub.u = 1;
    c[2].sub.u = 2;
    c[3].sub.u = 3;
  end

  final begin
    if (g[5].seen !== 30) $fatal(1, "g[5] read %0d from below, expected 30", g[5].seen);
    if (g[7].seen !== 50) $fatal(1, "g[7] read %0d from below, expected 50", g[7].seen);
    if (g[9].seen !== 70) $fatal(1, "g[9] read %0d from below, expected 70", g[9].seen);
    if (g[5].called !== 30) $fatal(1, "g[5] called below for %0d, expected 30", g[5].called);
    if (g[7].called !== 50) $fatal(1, "g[7] called below for %0d, expected 50", g[7].called);
    if (g[9].called !== 70) $fatal(1, "g[9] called below for %0d, expected 70", g[9].called);
    if (g[3].given !== 5) $fatal(1, "g[3] was given %0d, expected 5", g[3].given);
    if (g[5].given !== 7) $fatal(1, "g[5] was given %0d, expected 7", g[5].given);
    if (g[7].given !== 9) $fatal(1, "g[7] was given %0d, expected 9", g[7].given);
    if (g[9].given !== -1) $fatal(1, "g[9] was given %0d, expected none", g[9].given);
    if (g[5].driven !== 31) $fatal(1, "g[5] drove %0d, expected 31", g[5].driven);
    if (g[7].driven !== 51) $fatal(1, "g[7] drove %0d, expected 51", g[7].driven);
    if (g[9].driven !== 8) $fatal(1, "g[9] drove %0d, expected 8", g[9].driven);
    if (w[2].seen !== 203) $fatal(1, "w[2] read %0d, expected 203", w[2].seen);
    if (w[3].seen !== 307) $fatal(1, "w[3] read %0d, expected 307", w[3].seen);
    if (r[1].seen !== 3) $fatal(1, "r[1] read %0d, expected 3", r[1].seen);
    if (r[2].seen !== 7) $fatal(1, "r[2] read %0d, expected 7", r[2].seen);
    if (r[3].seen !== 15) $fatal(1, "r[3] read %0d, expected 15", r[3].seen);
    if (c[0].seen !== 100 || c[1].seen !== 100 || c[2].seen !== 100 ||
        c[3].seen !== 100)
      $fatal(1, "c read %0d %0d %0d %0d of its first block, expected 100",
             c[0].seen, c[1].seen, c[2].seen, c[3].seen);
    if (c[0].called !== 100 || c[1].called !== 100 || c[2].called !== 100 ||
        c[3].called !== 100)
      $fatal(1, "c called its first block for %0d %0d %0d %0d, expected 100",
             c[0].called, c[1].called, c[2].called, c[3].called);
    if (c[0].inner_seen !== 200 || c[1].inner_seen !== 200 ||
        c[2].inner_seen !== 200 || c[3].inner_seen !== 200)
      $fatal(1, "c read %0d %0d %0d %0d inside its first block, expected 200",
             c[0].inner_seen, c[1].inner_seen, c[2].inner_seen,
             c[3].inner_seen);
    if (outer_seen !== 41) $fatal(1, "outer.inner[1] read %0d, expected 41", outer_seen);
    if (k[1].seen !== 60 || k[2].seen !== 61)
      $fatal(1, "k's classes read %0d %0d below, expected 60 61", k[1].seen,
             k[2].seen);
    $display("All checks passed");
  end
endmodule
