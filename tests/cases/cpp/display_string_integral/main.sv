module Top;
  // LRM 5.9: SV string literals are packed bit vectors. LRM 21.2.1.1 %s
  // renders the operand as a string. When the operand is a packed integral
  // variable, the runtime needs the same PackedArray->String conversion the
  // $sscanf integral source path uses. This case proves it works end-to-end.
  logic [23:0] greeting = 24'h41_42_43;   // "ABC"
  bit   [31:0] tag      = 32'h54_45_53_54; // "TEST"

  initial begin
    $display("got: %s end", greeting);
    $display("[%s]", tag);
  end
endmodule
