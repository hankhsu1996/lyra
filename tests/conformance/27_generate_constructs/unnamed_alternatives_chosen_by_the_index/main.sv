// Alternatives of one conditional that the source leaves unnamed share the
// name LRM 27.6 gives them (`genblk1` here), and inside a loop the index
// selects a different one at different blocks (LRM 27.4, 27.5). Each is still
// its own block, holding its own instance, so every block builds what its own
// alternative wrote.
module Low (
    output int o
);
  assign o = 1;
endmodule

module High (
    output int o,
    input int x
);
  assign o = 2 + x;
endmodule

module Top;
  int seen[4];
  int cased[4];

  for (genvar i = 0; i < 4; i++) begin : kid
    if (i < 2) Low k (.o(seen[i]));
    else High k (.o(seen[i]), .x(i));
  end

  for (genvar i = 0; i < 4; i++) begin : item
    case (i)
      0, 1: Low k (.o(cased[i]));
      default: High k (.o(cased[i]), .x(10));
    endcase
  end

  initial begin
    #1;
    if (seen[0] !== 1 || seen[1] !== 1 || seen[2] !== 4 || seen[3] !== 5)
      $fatal(1, "the if blocks built %0d %0d %0d %0d", seen[0], seen[1],
             seen[2], seen[3]);
    if (cased[0] !== 1 || cased[1] !== 1 || cased[2] !== 12 || cased[3] !== 12)
      $fatal(1, "the case blocks built %0d %0d %0d %0d", cased[0], cased[1],
             cased[2], cased[3]);
    $display("All checks passed");
  end
endmodule
