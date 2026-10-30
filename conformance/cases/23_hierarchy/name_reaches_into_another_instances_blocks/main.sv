// A hierarchical name reaches any named declaration of the design (LRM 23.6),
// and a generate block is a scope on that path (LRM 23.9): a loop's block by
// the value its index stood at (LRM 27.4), a conditional's by the name of the
// alternative that elaborated, which several alternatives may share (LRM
// 27.5). So a name written in one instance reaches a variable and calls a
// subroutine declared in a block of another, the same way whether it descends
// into a child or climbs to an enclosing instance and descends from there.
// Each block of a loop answers by its own index; a conditional inside it
// selects per block, so what one block holds under a label another holds under
// the same label from a different alternative.
module Leaf;
  int from_above = 0;
  int called_above = 0;

  initial begin
    #1;
    from_above = Holder.g[2].arm.v;
    called_above = Holder.g[1].scaled(5);
  end
endmodule

module Holder;
  for (genvar i = 0; i < 4; i++) begin : g
    int seeded = (i + 1) * 10;

    function automatic int scaled(int n);
      return seeded * n;
    endfunction

    if (i % 2 == 0) begin : arm
      int v = 100 + i;
    end else begin : arm
      int v = 200 + i;
    end
  end

  Leaf leaf();
endmodule

module Top;
  Holder h();

  int even_arm = 0;
  int odd_arm = 0;
  int called = 0;
  int reseeded = 0;

  initial begin
    #1;
    even_arm = h.g[0].arm.v;
    odd_arm = h.g[3].arm.v;
    called = h.g[2].scaled(3);
    h.g[3].seeded = 7;
    reseeded = h.g[3].scaled(1);
  end

  final begin
    if (even_arm !== 100)
      $fatal(1, "h.g[0].arm.v was %0d, expected 100", even_arm);
    if (odd_arm !== 203)
      $fatal(1, "h.g[3].arm.v was %0d, expected 203", odd_arm);
    if (called !== 90)
      $fatal(1, "h.g[2].scaled(3) was %0d, expected 90", called);
    if (h.leaf.from_above !== 102)
      $fatal(1, "Holder.g[2].arm.v read %0d, expected 102", h.leaf.from_above);
    if (h.leaf.called_above !== 100)
      $fatal(1, "Holder.g[1].scaled(5) was %0d, expected 100",
             h.leaf.called_above);
    if (reseeded !== 7)
      $fatal(1, "a write to h.g[3].seeded left h.g[3].scaled(1) at %0d, expected 7",
             reseeded);
    $display("All checks passed");
  end
endmodule
