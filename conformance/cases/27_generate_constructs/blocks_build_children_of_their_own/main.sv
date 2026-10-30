// Each block of a loop is a scope of its own (LRM 27.4), so an instance one
// declares is that block's own, built with the values that block hands it (LRM
// 23.10): a child sized by the index is of a different shape at each index,
// while children handed alike are alike. The same holds of an instance a block
// declares inside a construct of its own -- a nested loop, and the alternative
// a condition on the index chose (LRM 27.5) -- and of a block reading what its
// neighbour's child holds.
module Leaf #(parameter int W = 1, parameter int V = 0) (output int width,
                                                       output int value);
  logic [W-1:0] held;
  assign width = $bits(held);
  assign value = V;
endmodule

module Top;
  for (genvar i = 0; i < 4; i++) begin : g
    int width, value, inner_value, chosen_value, neighbour_value = -1;

    // Sized by the index at the even blocks only, so the odd ones are alike.
    Leaf #(.W(i % 2 == 0 ? i + 1 : 1), .V(i * 10)) leaf (width, value);

    for (genvar j = 0; j < 2; j++) begin : inner
      int got;
      Leaf #(.V(i * 100 + j)) leaf (, got);
    end
    assign inner_value = inner[1].got;

    if (i > 1) begin : high
      Leaf #(.V(i + 1000)) leaf (, chosen_value);
    end else begin : low
      Leaf #(.V(i + 2000)) leaf (, chosen_value);
    end

    if (i > 0) begin : rest
      initial #1 neighbour_value = g[i-1].leaf.value;
    end
  end

  final begin
    if (g[0].width !== 1 || g[1].width !== 1 || g[2].width !== 3 ||
        g[3].width !== 1)
      $fatal(1, "child widths %0d %0d %0d %0d", g[0].width, g[1].width,
             g[2].width, g[3].width);
    if (g[0].value !== 0 || g[1].value !== 10 || g[2].value !== 20 ||
        g[3].value !== 30)
      $fatal(1, "child values %0d %0d %0d %0d", g[0].value, g[1].value,
             g[2].value, g[3].value);
    if (g[0].inner_value !== 1 || g[1].inner_value !== 101 ||
        g[2].inner_value !== 201 || g[3].inner_value !== 301)
      $fatal(1, "nested child values %0d %0d %0d %0d", g[0].inner_value,
             g[1].inner_value, g[2].inner_value, g[3].inner_value);
    if (g[0].chosen_value !== 2000 || g[1].chosen_value !== 2001 ||
        g[2].chosen_value !== 1002 || g[3].chosen_value !== 1003)
      $fatal(1, "chosen child values %0d %0d %0d %0d", g[0].chosen_value,
             g[1].chosen_value, g[2].chosen_value, g[3].chosen_value);
    if (g[0].neighbour_value !== -1 || g[1].neighbour_value !== 0 ||
        g[2].neighbour_value !== 10 || g[3].neighbour_value !== 20)
      $fatal(1, "neighbour child values %0d %0d %0d %0d",
             g[0].neighbour_value, g[1].neighbour_value, g[2].neighbour_value,
             g[3].neighbour_value);
    $display("All checks passed");
  end
endmodule
