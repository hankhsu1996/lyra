// A downward hierarchical reference from an enclosing scope into a child
// instance's generate block. The reference crosses into the child's unit, so
// the generate block and its signal are reached by name (LRM 23.6, 27): a
// conditional block `leaf.g.x` and an indexed loop block `leaf.bank[i].y`.
module Leaf;
  if (1) begin : g
    int x;
  end
  for (genvar i = 0; i < 2; i = i + 1) begin : bank
    int y;
  end
endmodule

module Top;
  Leaf leaf();
  initial begin
    leaf.g.x = 5;
    leaf.bank[0].y = 7;
    leaf.bank[1].y = 9;
    #1;
    $display("%0d %0d %0d", leaf.g.x, leaf.bank[0].y, leaf.bank[1].y);
  end
endmodule
