// The body of a loop is a statement, so it may itself be a loop. The inner loop
// then runs to completion inside each pass of the outer one and starts again
// from its own initialization on the next pass, whichever two loop forms are
// combined (LRM 12.7).
module Top;
  int for_in_for;
  int while_in_while;
  int for_in_while;
  int while_in_for;

  initial begin
    int row;
    int col;

    for_in_for = 0;
    for (int i = 0; i < 3; i = i + 1)
      for (int j = 0; j < 3; j = j + 1)
        for_in_for = for_in_for + 1;

    while_in_while = 0;
    row = 0;
    while (row < 3) begin
      col = 0;
      while (col < 4) begin
        while_in_while = while_in_while + 1;
        col = col + 1;
      end
      row = row + 1;
    end

    for_in_while = 0;
    row = 0;
    while (row < 3) begin
      for (int c = 0; c < 4; c = c + 1) for_in_while = for_in_while + 1;
      row = row + 1;
    end

    while_in_for = 0;
    for (int r = 0; r < 3; r = r + 1) begin
      col = 0;
      while (col < 4) begin
        while_in_for = while_in_for + 1;
        col = col + 1;
      end
    end
  end

  final begin
    if (for_in_for !== 9)
      $fatal(1, "for_in_for was %0d, expected 9", for_in_for);
    if (while_in_while !== 12)
      $fatal(1, "while_in_while was %0d, expected 12", while_in_while);
    if (for_in_while !== 12)
      $fatal(1, "for_in_while was %0d, expected 12", for_in_while);
    if (while_in_for !== 12)
      $fatal(1, "while_in_for was %0d, expected 12", while_in_for);
    $display("All checks passed");
  end
endmodule
