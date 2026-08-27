// The new[] constructor takes an optional array to initialize the new
// elements from, and that array's size need not match the size operand: a
// longer one is truncated to the operand and a shorter one is padded out to
// it with the element type's default value, which is zero for a 2-state
// element and all x for a 4-state one. The array under construction may be
// its own initialization array, which is how a dynamic array is resized while
// keeping the elements it already held; the size operand is evaluated before
// the initialization array, so it may be computed from the array's current
// size (LRM 7.5.1, Table 7-1).
module Top;
  int src [] = '{2, 3, 4};
  logic [7:0] four_state_src [] = '{8'h11, 8'h22};

  int truncated [];
  int padded [];
  logic [7:0] four_state_padded [];
  int grown [];
  int shrunk [];

  int truncated_size;
  int truncated0 = 77;
  int truncated1 = 77;

  int padded_size;
  int padded0 = 77;
  int padded2 = 77;
  int padded3 = 77;

  logic [7:0] four_state_padded0 = 8'h5A;
  logic [7:0] four_state_padded2 = 8'h5A;
  logic [7:0] four_state_padded3 = 8'h5A;

  int grown_size;
  int grown0 = 77;
  int grown2 = 77;
  int grown3 = 77;
  int grown5 = 77;

  int shrunk_size;
  int shrunk0 = 77;
  int shrunk1 = 77;

  initial begin
    truncated = new[5];
    truncated[0] = 81;
    truncated[1] = 82;
    truncated[2] = 83;
    truncated[3] = 84;
    truncated[4] = 85;
    truncated = new[2](src);
    truncated_size = truncated.size();
    truncated0 = truncated[0];
    truncated1 = truncated[1];

    padded = new[4];
    padded[0] = 91;
    padded[1] = 92;
    padded[2] = 93;
    padded[3] = 94;
    padded = new[4](src);
    padded_size = padded.size();
    padded0 = padded[0];
    padded2 = padded[2];
    padded3 = padded[3];

    four_state_padded = new[4];
    four_state_padded[2] = 8'hAA;
    four_state_padded[3] = 8'hBB;
    four_state_padded = new[4](four_state_src);
    four_state_padded0 = four_state_padded[0];
    four_state_padded2 = four_state_padded[2];
    four_state_padded3 = four_state_padded[3];

    grown = new[3];
    grown[0] = 5;
    grown[1] = 6;
    grown[2] = 7;
    grown = new[grown.size() * 2](grown);
    grown_size = grown.size();
    grown0 = grown[0];
    grown2 = grown[2];
    grown3 = grown[3];
    grown5 = grown[5];

    shrunk = new[4];
    shrunk[0] = 1;
    shrunk[1] = 2;
    shrunk[2] = 3;
    shrunk[3] = 4;
    shrunk = new[2](shrunk);
    shrunk_size = shrunk.size();
    shrunk0 = shrunk[0];
    shrunk1 = shrunk[1];
  end

  final begin
    if (truncated_size !== 2)
      $fatal(1, "truncated_size was %0d, expected 2", truncated_size);
    if (truncated0 !== 2)
      $fatal(1, "truncated0 was %0d, expected 2", truncated0);
    if (truncated1 !== 3)
      $fatal(1, "truncated1 was %0d, expected 3", truncated1);

    if (padded_size !== 4)
      $fatal(1, "padded_size was %0d, expected 4", padded_size);
    if (padded0 !== 2) $fatal(1, "padded0 was %0d, expected 2", padded0);
    if (padded2 !== 4) $fatal(1, "padded2 was %0d, expected 4", padded2);
    if (padded3 !== 0) $fatal(1, "padded3 was %0d, expected 0", padded3);

    if (four_state_padded0 !== 8'h11)
      $fatal(1, "four_state_padded0 was %0h, expected 11",
             four_state_padded0);
    if (four_state_padded2 !== 8'bxxxxxxxx)
      $fatal(1, "four_state_padded2 was %0h, expected all x",
             four_state_padded2);
    if (four_state_padded3 !== 8'bxxxxxxxx)
      $fatal(1, "four_state_padded3 was %0h, expected all x",
             four_state_padded3);

    if (grown_size !== 6)
      $fatal(1, "grown_size was %0d, expected 6", grown_size);
    if (grown0 !== 5) $fatal(1, "grown0 was %0d, expected 5", grown0);
    if (grown2 !== 7) $fatal(1, "grown2 was %0d, expected 7", grown2);
    if (grown3 !== 0) $fatal(1, "grown3 was %0d, expected 0", grown3);
    if (grown5 !== 0) $fatal(1, "grown5 was %0d, expected 0", grown5);

    if (shrunk_size !== 2)
      $fatal(1, "shrunk_size was %0d, expected 2", shrunk_size);
    if (shrunk0 !== 1) $fatal(1, "shrunk0 was %0d, expected 1", shrunk0);
    if (shrunk1 !== 2) $fatal(1, "shrunk1 was %0d, expected 2", shrunk1);
    $display("All checks passed");
  end
endmodule
