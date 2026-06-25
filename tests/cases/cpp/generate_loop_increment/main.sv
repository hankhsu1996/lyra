module Top;
  logic [3:0] inc;
  logic [3:0] dec;
  logic [3:0] compound;
  logic [3:0] assign_form;

  // Every genvar generate-loop iteration form (LRM 27.4). Each instance drives
  // the bit at its own genvar value, so a fully-set result proves the loop
  // visited every index exactly once with the right step.
  for (genvar i = 0; i < 4; i++) begin : g_inc
    assign inc[i] = 1'b1;
  end
  for (genvar i = 3; i >= 0; i--) begin : g_dec
    assign dec[i] = 1'b1;
  end
  for (genvar i = 0; i < 4; i += 1) begin : g_compound
    assign compound[i] = 1'b1;
  end
  for (genvar i = 0; i < 4; i = i + 1) begin : g_assign
    assign assign_form[i] = 1'b1;
  end
endmodule
