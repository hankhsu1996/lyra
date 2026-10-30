// An unbased unsized literal is as wide as the context it stands in and sets
// every bit of it (LRM 5.7.1), so where it is assigned to a part-select a
// loop's index places (LRM 11.5.1, 27.4), it is the same fill in every block:
// what changes between blocks is where the select lands, not what is written.
// Each block clears its own word and sets its own byte, one in a procedure and
// one in a continuous assignment.
module Top;
  logic [63:0] cleared;
  logic [31:0] set;
  logic [3:0] pick;

  for (genvar i = 0; i < 4; i++) begin : word
    always_comb begin
      cleared[i*16+:16] = '1;
      if (pick[i]) cleared[i*16+:16] = '0;
    end
    assign set[i*8+:8] = pick[i] ? '1 : '0;
  end

  initial begin
    pick = 4'b0101;
    #1;
    if (cleared !== 64'hffff_0000_ffff_0000)
      $fatal(1, "the cleared words are %h", cleared);
    if (set !== 32'h00ff_00ff) $fatal(1, "the set bytes are %h", set);
    $display("All checks passed");
  end
endmodule
