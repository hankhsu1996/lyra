// A genvar is used as an integer during elaboration and does not exist at
// simulation time. Within the generate block of a loop generate construct the
// loop index name denotes an implicit localparam whose value in each instance
// is the value the index held when that instance was elaborated (LRM 27.4). A
// nested loop generate can therefore bound its own scheme with the enclosing
// loop's index, and each enclosing instance elaborates a different number of
// inner instances.
module Top;
  bit [3:0] driven [3];

  for (genvar stage = 0; stage < 3; stage++) begin : g_stage
    for (genvar seg = 0; seg < 4 - stage; seg++) begin : g_seg
      assign driven[stage][seg] = 1'b1;
    end
  end

  final begin
    if (driven[0] !== 4'b1111)
      $fatal(1, "stage 0 drove %b, expected 1111", driven[0]);
    if (driven[1] !== 4'b0111)
      $fatal(1, "stage 1 drove %b, expected 0111", driven[1]);
    if (driven[2] !== 4'b0011)
      $fatal(1, "stage 2 drove %b, expected 0011", driven[2]);
    $display("All checks passed");
  end
endmodule
