// A loop executes its body in full on every pass, so a conditional inside the
// body is evaluated afresh each time and may select a different branch on
// different passes (LRM 12.7).
module Top;
  int for_then_hits;
  int for_else_hits;
  int while_then_hits;
  int while_else_hits;

  initial begin
    int i;

    for_then_hits = 0;
    for_else_hits = 0;
    for (int k = 0; k < 6; k = k + 1) begin
      if (k > 0)
        for_then_hits = for_then_hits + 1;
      else
        for_else_hits = for_else_hits + 1;
    end

    i = 0;
    while_then_hits = 0;
    while_else_hits = 0;
    while (i < 10) begin
      if (i < 5)
        while_then_hits = while_then_hits + 1;
      else
        while_else_hits = while_else_hits + 1;
      i = i + 1;
    end
  end

  final begin
    if (for_then_hits !== 5)
      $fatal(1, "for_then_hits was %0d, expected 5", for_then_hits);
    if (for_else_hits !== 1)
      $fatal(1, "for_else_hits was %0d, expected 1", for_else_hits);
    if (while_then_hits !== 5)
      $fatal(1, "while_then_hits was %0d, expected 5", while_then_hits);
    if (while_else_hits !== 5)
      $fatal(1, "while_else_hits was %0d, expected 5", while_else_hits);
    $display("All checks passed");
  end
endmodule
