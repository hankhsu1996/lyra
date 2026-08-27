// The break statement jumps out of the loop it stands in and no further, so an
// enclosing loop is unaffected and goes on to its next pass (LRM 12.8).
module Top;
  int outer_passes;
  int inner_hits;
  int forever_passes;
  int inner_total;

  initial begin
    outer_passes = 0;
    inner_hits = 0;
    for (int i = 0; i < 2; i = i + 1) begin
      automatic int j = 0;
      outer_passes = outer_passes + 1;
      while (j < 3) begin
        if (j == 1) break;
        inner_hits = inner_hits + 1;
        j = j + 1;
      end
    end

    forever_passes = 0;
    inner_total = 0;
    forever begin
      int inner;
      forever_passes = forever_passes + 1;
      inner = 0;
      forever begin
        inner = inner + 1;
        if (inner == 2) break;
      end
      inner_total = inner_total + inner;
      if (forever_passes == 3) break;
    end
  end

  final begin
    if (outer_passes !== 2)
      $fatal(1, "outer_passes was %0d, expected 2", outer_passes);
    if (inner_hits !== 2)
      $fatal(1, "inner_hits was %0d, expected 2", inner_hits);
    if (forever_passes !== 3)
      $fatal(1, "forever_passes was %0d, expected 3", forever_passes);
    if (inner_total !== 6)
      $fatal(1, "inner_total was %0d, expected 6", inner_total);
    $display("All checks passed");
  end
endmodule
