// Inside a loop generate's block, the index names the implicit localparam
// LRM 27.4 declares in that block, usable anywhere within it a parameter is --
// including inside a further generate construct the block holds. So a
// conditional written inside another conditional (LRM 27.5), a case construct's
// selector and labels, a loop's bounds, and a constant the block settles from
// the index are all evaluated for the block they stand in, and every block
// selects and counts by its own index.
module Top;
  // Two levels of conditional under the loop, the inner one reading the index.
  int two_level [4] = '{-1, -1, -1, -1};
  for (genvar i = 0; i < 4; i++) begin : chain
    if (i < 2) begin : low
      initial two_level[i] = i;
    end else begin : high
      if (i == 3) begin : last
        initial two_level[i] = 30 + i;
      end else begin : middle
        initial two_level[i] = 20 + i;
      end
    end
  end

  // A conditional inside an inner loop's block, reading the outer index.
  int diagonal [3][3] = '{'{-1, -1, -1}, '{-1, -1, -1}, '{-1, -1, -1}};
  for (genvar r = 0; r < 3; r++) begin : row
    for (genvar c = 0; c < 3; c++) begin : col
      if (r == c) begin : on
        initial diagonal[r][c] = 1;
      end else begin : off
        initial diagonal[r][c] = 0;
      end
    end
  end

  // A loop whose bound reads the index, inside a named block of the block.
  int triangle [4][4] = '{'{-1, -1, -1, -1}, '{-1, -1, -1, -1},
                          '{-1, -1, -1, -1}, '{-1, -1, -1, -1}};
  for (genvar t = 1; t < 4; t++) begin : tri_row
    begin : inner
      for (genvar u = 0; u < t; u++) begin : tri_col
        initial triangle[t][u] = t * 10 + u;
      end
    end
  end

  // A conditional reading a constant the block settled from its index.
  int derived [3] = '{-1, -1, -1};
  for (genvar k = 0; k < 3; k++) begin : scaled
    localparam int K = k * 2;
    begin : holder
      if (K > 2) begin : above
        initial derived[k] = K;
      end else begin : below
        initial derived[k] = 100 + K;
      end
    end
  end

  // A case construct whose selector reads the index and whose label reads a
  // constant the block settled from it.
  int picked [3] = '{-1, -1, -1};
  for (genvar p = 0; p < 3; p++) begin : sorted
    localparam int Last = 2;
    begin : chooser
      case (p)
        0: begin : first
          initial picked[p] = 10;
        end
        Last: begin : final_one
          initial picked[p] = 30;
        end
        default: begin : other
          initial picked[p] = 20;
        end
      endcase
    end
  end

  final begin
    if (picked[0] !== 10) $fatal(1, "picked[0] was %0d, expected 10", picked[0]);
    if (picked[1] !== 20) $fatal(1, "picked[1] was %0d, expected 20", picked[1]);
    if (picked[2] !== 30) $fatal(1, "picked[2] was %0d, expected 30", picked[2]);

    if (two_level[0] !== 0) $fatal(1, "two_level[0] was %0d, expected 0", two_level[0]);
    if (two_level[1] !== 1) $fatal(1, "two_level[1] was %0d, expected 1", two_level[1]);
    if (two_level[2] !== 22) $fatal(1, "two_level[2] was %0d, expected 22", two_level[2]);
    if (two_level[3] !== 33) $fatal(1, "two_level[3] was %0d, expected 33", two_level[3]);

    foreach (diagonal[r, c]) begin
      if (diagonal[r][c] !== ((r == c) ? 1 : 0))
        $fatal(1, "diagonal[%0d][%0d] was %0d", r, c, diagonal[r][c]);
    end

    foreach (triangle[t, u]) begin
      if (t >= 1 && u < t) begin
        if (triangle[t][u] !== t * 10 + u)
          $fatal(1, "triangle[%0d][%0d] was %0d, expected %0d", t, u,
                 triangle[t][u], t * 10 + u);
      end else if (triangle[t][u] !== -1) begin
        $fatal(1, "triangle[%0d][%0d] was %0d, but no block stands there", t,
               u, triangle[t][u]);
      end
    end

    if (derived[0] !== 100) $fatal(1, "derived[0] was %0d, expected 100", derived[0]);
    if (derived[1] !== 102) $fatal(1, "derived[1] was %0d, expected 102", derived[1]);
    if (derived[2] !== 4) $fatal(1, "derived[2] was %0d, expected 4", derived[2]);
    $display("All checks passed");
  end
endmodule
