// A genvar is used as an integer during elaboration and does not exist at
// simulation time. Within the generate block of a loop generate construct the
// loop index name denotes an implicit localparam whose value in each instance
// is the value the index held when that instance was elaborated (LRM 27.4). A
// nested loop generate can therefore bound its own scheme with the enclosing
// loop's index, and each enclosing instance elaborates a different number of
// inner instances -- none at all where the scheme counts none out. Each inner
// instance is still its own scope, named by the value its own index stood at
// (LRM 23.6) wherever its loop started, and holds what its own index fixed.
module Reader;
  int rest_of_stage_1, rest_of_stage_2;
  initial begin
    rest_of_stage_1 = Top.g_stage[1].g_rest[3].mark;
    rest_of_stage_2 = Top.g_stage[2].g_rest[2].mark;
  end
endmodule

module Top;
  bit [3:0] driven [3];
  Reader reader ();

  for (genvar stage = 0; stage < 3; stage++) begin : g_stage
    for (genvar seg = 0; seg < 4 - stage; seg++) begin : g_seg
      assign driven[stage][seg] = 1'b1;
    end
    for (genvar seg = 4 - stage; seg < 4; seg++) begin : g_rest
      int mark = 100 * stage + seg;
      string where;
      initial where = $sformatf("%m");
    end
    for (genvar bits = 0; bits <= stage; bits++) begin : g_wide
      logic [bits:0] ones;
      int width;
      initial begin
        ones = '1;
        width = $bits(ones);
      end
    end
  end

  final begin
    if (driven[0] !== 4'b1111)
      $fatal(1, "stage 0 drove %b, expected 1111", driven[0]);
    if (driven[1] !== 4'b0111)
      $fatal(1, "stage 1 drove %b, expected 0111", driven[1]);
    if (driven[2] !== 4'b0011)
      $fatal(1, "stage 2 drove %b, expected 0011", driven[2]);

    if (g_stage[1].g_rest[3].mark !== 103)
      $fatal(1, "stage 1 rest 3 holds %0d", g_stage[1].g_rest[3].mark);
    if (g_stage[2].g_rest[2].mark !== 202)
      $fatal(1, "stage 2 rest 2 holds %0d", g_stage[2].g_rest[2].mark);
    if (g_stage[2].g_rest[3].mark !== 203)
      $fatal(1, "stage 2 rest 3 holds %0d", g_stage[2].g_rest[3].mark);
    if (g_stage[2].g_rest[3].where != "Top.g_stage[2].g_rest[3]")
      $fatal(1, "stage 2 rest 3 calls itself %s", g_stage[2].g_rest[3].where);
    if (reader.rest_of_stage_1 !== 103 || reader.rest_of_stage_2 !== 202)
      $fatal(
          1, "a name from another module read %0d and %0d",
          reader.rest_of_stage_1, reader.rest_of_stage_2);

    if (g_stage[0].g_wide[0].width !== 1)
      $fatal(1, "stage 0 wide 0 is %0d bits", g_stage[0].g_wide[0].width);
    if (g_stage[2].g_wide[1].width !== 2 || g_stage[2].g_wide[2].width !== 3)
      $fatal(
          1, "stage 2 wide 1 and 2 are %0d and %0d bits",
          g_stage[2].g_wide[1].width, g_stage[2].g_wide[2].width);
    if (g_stage[1].g_wide[1].ones !== 2'b11)
      $fatal(1, "stage 1 wide 1 holds %b", g_stage[1].g_wide[1].ones);
    if (g_stage[2].g_wide[2].ones !== 3'b111)
      $fatal(1, "stage 2 wide 2 holds %b", g_stage[2].g_wide[2].ones);
    $display("All checks passed");
  end
endmodule
