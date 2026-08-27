// Every genvar loop-generate increment form visits each index exactly once,
// and within each generated block the loop index names an implicit localparam
// whose value is the index that block was elaborated with (LRM 27.4). Each
// generated assignment therefore drives the bit at its own index with a value
// that index decides, so the result records which index every block was given
// and not merely that a block was made for every index.
module Top;
  logic [3:0] inc;
  logic [3:0] dec;
  logic [3:0] compound;
  logic [3:0] assign_form;

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

  final begin
    if (inc !== 4'b1011) $fatal(1, "inc=%b, expected 1011", inc);
    if (dec !== 4'b1100) $fatal(1, "dec=%b, expected 1100", dec);
    if (compound !== 4'b0101)
      $fatal(1, "compound=%b, expected 0101", compound);
    if (assign_form !== 4'b0001)
      $fatal(1, "assign_form=%b, expected 0001", assign_form);
    $display("All checks passed");
  end
endmodule
