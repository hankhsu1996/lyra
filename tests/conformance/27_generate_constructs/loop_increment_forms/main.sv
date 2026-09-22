// Every form LRM 27.4 admits for a loop generate's iteration step visits each
// index its scheme reaches exactly once, and within each generated block the
// loop index names an implicit localparam whose value is the index that block
// was elaborated with. Each generated assignment therefore drives the bit at
// its own index with a value that index decides, so the result records which
// index every block was given and not merely that a block was made.
//
// The step need not move by one. The same clause says the index values "do not
// have to form a contiguous range of integers", so a step that scales or shifts
// leaves a sparse array of blocks: the bits no block stood at are driven by
// nothing and stay undriven, which is what distinguishes the indices the scheme
// reached from the range it ran over.
module Top;
  logic [3:0] inc;
  logic [3:0] dec;
  logic [3:0] compound;
  logic [3:0] assign_form;
  logic [15:0] scaled;
  logic [15:0] shifted;

  for (genvar i = 0; i < 4; i++) begin : g_inc
    assign inc[i] = (i != 2);
  end
  for (genvar i = 3; i >= 0; i--) begin : g_dec
    assign dec[i] = (i > 1);
  end
  for (genvar i = 0; i < 4; i += 1) begin : g_compound
    assign compound[i] = (i % 2 == 0);
  end
  for (genvar i = 0; i < 4; i = i + 1) begin : g_assign
    assign assign_form[i] = (i < 1);
  end
  for (genvar i = 1; i < 16; i *= 2) begin : g_scaled
    assign scaled[i] = 1'b1;
  end
  for (genvar i = 1; i < 16; i <<= 2) begin : g_shifted
    assign shifted[i] = 1'b1;
  end

  final begin
    if (inc !== 4'b1011) $fatal(1, "inc=%b, expected 1011", inc);
    if (dec !== 4'b1100) $fatal(1, "dec=%b, expected 1100", dec);
    if (compound !== 4'b0101)
      $fatal(1, "compound=%b, expected 0101", compound);
    if (assign_form !== 4'b0001)
      $fatal(1, "assign_form=%b, expected 0001", assign_form);
    // Blocks stood at 1, 2, 4 and 8; every other bit was reached by no block.
    if (scaled !== 16'bxxxx_xxx1_xxx1_x11x)
      $fatal(1, "scaled=%b, expected xxxxxxx1xxx1x11x", scaled);
    // Blocks stood at 1 and 4.
    if (shifted !== 16'bxxxx_xxxx_xxx1_xx1x)
      $fatal(1, "shifted=%b, expected xxxxxxxxxxx1xx1x", shifted);
    $display("All checks passed");
  end
endmodule
