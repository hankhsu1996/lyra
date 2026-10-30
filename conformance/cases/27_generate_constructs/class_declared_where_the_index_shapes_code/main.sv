// A generate block may declare a class (Syntax 27-1), and a method of it
// reaches what the block reaches: a name of the block, and a name searched
// upward from it (LRM 23.8). Where the loop's index also sizes what the block's
// process selects (LRM 27.4), each block still holds the class it declared and
// each method still lands where its own block's names do. A module declaring a
// class while its parameter sizes what its process selects (LRM 23.10) is the
// same case one level out.
module Peer;
  int base = 7;
endmodule

module Leaf #(parameter int P = 1) (output int got, output logic [7:0] cut);
  logic [7:0] all_set = 8'hff;

  class Probe;
    function int read();
      return Top.peer.base + P;
    endfunction
  endclass

  initial begin
    Probe probe = new;
    #1;
    cut = 8'(all_set[P:0]);
    got = probe.read();
  end
endmodule

module Top;
  Peer peer ();
  int got [1:2];
  logic [7:0] cut [1:2];

  for (genvar i = 1; i < 3; i++) begin : g
    logic [7:0] all_set = 8'hff;

    class Probe;
      function int read();
        return Top.peer.base + i;
      endfunction
    endclass

    initial begin
      Probe probe = new;
      #1;
      cut[i] = 8'(all_set[i:0]);
      got[i] = probe.read();
    end
  end

  int leaf_got [1:2];
  logic [7:0] leaf_cut [1:2];
  Leaf #(.P(1)) narrow (.got(leaf_got[1]), .cut(leaf_cut[1]));
  Leaf #(.P(2)) wide (.got(leaf_got[2]), .cut(leaf_cut[2]));

  final begin
    for (int k = 1; k < 3; k++) begin
      if (got[k] !== 7 + k)
        $fatal(1, "block %0d's class read %0d, expected %0d", k, got[k], 7 + k);
      if (cut[k] !== 8'((1 << (k + 1)) - 1))
        $fatal(1, "block %0d's process cut %h", k, cut[k]);
      if (leaf_got[k] !== 7 + k)
        $fatal(1, "leaf %0d's class read %0d, expected %0d", k, leaf_got[k],
               7 + k);
      if (leaf_cut[k] !== 8'((1 << (k + 1)) - 1))
        $fatal(1, "leaf %0d's process cut %h", k, leaf_cut[k]);
    end
    $display("All checks passed");
  end
endmodule
