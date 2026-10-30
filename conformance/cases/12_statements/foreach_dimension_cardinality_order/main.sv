// Several loop variables in a foreach-loop correspond to nested loops, and
// which variable gets which loop is settled by dimension cardinality: the
// slowest-varying dimension is dimension 1 and is the outermost loop, and each
// higher-numbered dimension changes more rapidly, so the last loop variable is
// the innermost. Among the dimensions of one declaration the rightmost varies
// most rapidly, except that every packed dimension varies more rapidly than
// any unpacked one, which puts the unpacked dimensions of a declaration ahead
// of its packed ones (LRM 12.7.3, LRM 20.7, LRM 7.4.4).
module Top;
  int plane [2][3];
  int plane_passes;

  int cube [2][3][4];
  int cube_passes;

  bit [7:0] words [3];
  int word_visits [24];
  int word_passes;

  initial begin
    plane_passes = 0;
    foreach (plane[i, j]) begin
      plane[i][j] = plane_passes;
      plane_passes = plane_passes + 1;
    end

    cube_passes = 0;
    foreach (cube[i, j, k]) begin
      cube[i][j][k] = cube_passes;
      cube_passes = cube_passes + 1;
    end

    word_passes = 0;
    foreach (words[i, j]) begin
      word_visits[word_passes] = i * 10 + j;
      word_passes = word_passes + 1;
    end
  end

  final begin
    if (plane_passes !== 6)
      $fatal(1, "plane_passes was %0d, expected 6", plane_passes);
    if (plane[0][0] !== 0)
      $fatal(1, "plane[0][0] was %0d, expected 0", plane[0][0]);
    if (plane[0][2] !== 2)
      $fatal(1, "plane[0][2] was %0d, expected 2", plane[0][2]);
    if (plane[1][0] !== 3)
      $fatal(1, "plane[1][0] was %0d, expected 3", plane[1][0]);
    if (plane[1][2] !== 5)
      $fatal(1, "plane[1][2] was %0d, expected 5", plane[1][2]);
    if (cube_passes !== 24)
      $fatal(1, "cube_passes was %0d, expected 24", cube_passes);
    if (cube[0][0][0] !== 0)
      $fatal(1, "cube[0][0][0] was %0d, expected 0", cube[0][0][0]);
    if (cube[0][0][3] !== 3)
      $fatal(1, "cube[0][0][3] was %0d, expected 3", cube[0][0][3]);
    if (cube[0][1][0] !== 4)
      $fatal(1, "cube[0][1][0] was %0d, expected 4", cube[0][1][0]);
    if (cube[1][0][0] !== 12)
      $fatal(1, "cube[1][0][0] was %0d, expected 12", cube[1][0][0]);
    if (cube[1][2][3] !== 23)
      $fatal(1, "cube[1][2][3] was %0d, expected 23", cube[1][2][3]);
    if (word_passes !== 24)
      $fatal(1, "word_passes was %0d, expected 24", word_passes);
    if (word_visits[0] !== 7)
      $fatal(1, "word_visits[0] was %0d, expected 7", word_visits[0]);
    if (word_visits[1] !== 6)
      $fatal(1, "word_visits[1] was %0d, expected 6", word_visits[1]);
    if (word_visits[7] !== 0)
      $fatal(1, "word_visits[7] was %0d, expected 0", word_visits[7]);
    if (word_visits[8] !== 17)
      $fatal(1, "word_visits[8] was %0d, expected 17", word_visits[8]);
    if (word_visits[23] !== 20)
      $fatal(1, "word_visits[23] was %0d, expected 20", word_visits[23]);
    $display("All checks passed");
  end
endmodule
