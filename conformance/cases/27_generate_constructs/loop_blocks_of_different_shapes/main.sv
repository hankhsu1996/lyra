// A loop generate's blocks may declare different things -- here a width each
// block's own index fixes (LRM 27.4) -- and each block is still its own scope:
// it holds its own variables at the width its index gave them, is named by its
// index in a hierarchical path (LRM 23.6) and in %m (LRM 21.2.1.6), and an
// instance it declares is built with the value its index hands it.
module Leaf #(parameter int K = 0);
  int k = K;
endmodule

module Top;
  for (genvar i = 0; i < 6; i += 1) begin : g
    logic [i % 2 + 1:0] wide;
    int width;
    string where;
    Leaf #(.K(i * 10)) u ();
    initial begin
      wide = '1;
      width = $bits(wide);
      where = $sformatf("%m");
    end
  end

  final begin
    for (int n = 0; n < 6; n++) begin
      int expected;
      expected = n % 2 == 0 ? 2 : 3;
      case (n)
        0: if (g[0].width !== expected) $fatal(1, "g[0] is %0d bits", g[0].width);
        1: if (g[1].width !== expected) $fatal(1, "g[1] is %0d bits", g[1].width);
        2: if (g[2].width !== expected) $fatal(1, "g[2] is %0d bits", g[2].width);
        3: if (g[3].width !== expected) $fatal(1, "g[3] is %0d bits", g[3].width);
        4: if (g[4].width !== expected) $fatal(1, "g[4] is %0d bits", g[4].width);
        5: if (g[5].width !== expected) $fatal(1, "g[5] is %0d bits", g[5].width);
      endcase
    end
    if (g[0].wide !== 2'b11) $fatal(1, "g[0].wide holds %b", g[0].wide);
    if (g[3].wide !== 3'b111) $fatal(1, "g[3].wide holds %b", g[3].wide);
    if (g[4].where != "Top.g[4]") $fatal(1, "g[4] calls itself %s", g[4].where);
    if (g[5].where != "Top.g[5]") $fatal(1, "g[5] calls itself %s", g[5].where);
    if (g[2].u.k !== 20 || g[5].u.k !== 50)
      $fatal(1, "g[2].u and g[5].u read %0d and %0d", g[2].u.k, g[5].u.k);
    $display("All checks passed");
  end
endmodule
