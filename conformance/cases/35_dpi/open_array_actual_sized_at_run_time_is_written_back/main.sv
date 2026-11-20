// An actual is associated with a formal by the rules that govern assigning
// one array to another, whatever the formal's direction, and an open array
// formal's actual follows the same rules (LRM 7.7, 35.6.1.1). So a dynamic
// array is an actual for an `inout` and an `output` open array as for an
// `input` one: the foreign side sees the actual's own range, and what it
// writes there is the actual's value after the call, at the size the actual
// had (LRM 35.6.2, Annex H.7.6, H.12).
// An actual whose elements are themselves fixed-size arrays has as many
// elements after the call as before, each written back whole.
module Top;
  import "DPI-C" function void doubled(inout byte data[]);
  import "DPI-C" function void numbered(output byte data[]);
  import "DPI-C" function void rows_doubled(inout byte data[][]);

  byte grown[];
  byte filled[];
  byte rows[][2];

  initial begin
    grown = new[3];
    grown[0] = 5;
    grown[1] = 6;
    grown[2] = 7;
    doubled(grown);

    filled = new[4];
    numbered(filled);

    rows = new[3];
    foreach (rows[i, j]) rows[i][j] = byte'(i * 2 + j + 1);
    rows_doubled(rows);
  end

  final begin
    if (grown.size() !== 3) $fatal(1, "grown has %0d elements", grown.size());
    if (grown[0] !== 10 || grown[1] !== 12 || grown[2] !== 14)
      $fatal(1, "grown is %p, expected '{10, 12, 14}", grown);
    if (filled.size() !== 4)
      $fatal(1, "filled has %0d elements", filled.size());
    for (int i = 0; i < 4; i++)
      if (filled[i] !== i + 1)
        $fatal(1, "filled[%0d] is %0d, expected %0d", i, filled[i], i + 1);
    if (rows.size() !== 3) $fatal(1, "rows has %0d elements", rows.size());
    foreach (rows[i, j])
      if (rows[i][j] !== byte'((i * 2 + j + 1) * 2))
        $fatal(1, "rows[%0d][%0d] is %0d, expected %0d", i, j, rows[i][j],
               (i * 2 + j + 1) * 2);
    $display("All checks passed");
  end
endmodule
