// An upward hierarchical reference written inside a generate block resolves the
// same as one in the module body: its ExternUp member rides the generate-scope
// class and climbs that object's own parent chain at Bind (LRM 23.8). Covers a
// conditional block, a loop block, several blocks referencing the same ancestor
// signal (each gets its own slot in its own scope), and an upward write.
module Leaf;
  if (1) begin : gi
    initial begin #1; $display("gi=%0d", Top.sig); end
  end
  if (1) begin : gj
    initial begin #2; $display("gj=%0d", Top.sig); end
  end
  for (genvar k = 0; k < 2; k = k + 1) begin : gf
    int x;
    always_comb x = Top.sig;
  end
  initial begin #3; $display("gf=%0d,%0d", gf[0].x, gf[1].x); end
  if (1) begin : gw
    initial begin #4; Top.sig = 99; end
  end
endmodule

module Top;
  int sig;
  Leaf leaf();
  initial begin
    sig = 42;
    #5;
    $display("sig=%0d", sig);
  end
endmodule
