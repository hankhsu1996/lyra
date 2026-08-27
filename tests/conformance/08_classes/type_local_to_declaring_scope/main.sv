// A class declared inside a module is a type belonging to that instance, and
// a generate block is likewise a scope of its own, so the same class name
// declared in two such scopes gives two unrelated types, each carrying only
// its own members. A type shared between scopes has to be declared where
// both can see it -- in a package, or at the compilation unit level --
// instead (LRM 6.22, 27.5).
module Alpha (output int out);
  class Packet;
    int alpha_only = 11;
  endclass

  Packet p = new();

  initial out = p.alpha_only;
endmodule

module Beta (output int out);
  class Packet;
    int beta_only = 22;
  endclass

  Packet p = new();

  initial out = p.beta_only;
endmodule

module Top;
  int alpha_v;
  int beta_v;
  int first_block_v;
  int second_block_v;

  Alpha a (.out(alpha_v));
  Beta b (.out(beta_v));

  if (1) begin : g1
    class Packet;
      int g1_only = 33;
    endclass

    Packet p = new();

    initial first_block_v = p.g1_only;
  end

  if (1) begin : g2
    class Packet;
      int g2_only = 44;
    endclass

    Packet p = new();

    initial second_block_v = p.g2_only;
  end

  final begin
    if (alpha_v !== 11) $fatal(1, "alpha_v was %0d, expected 11", alpha_v);
    if (beta_v !== 22) $fatal(1, "beta_v was %0d, expected 22", beta_v);
    if (first_block_v !== 33)
      $fatal(1, "first_block_v was %0d, expected 33", first_block_v);
    if (second_block_v !== 44)
      $fatal(1, "second_block_v was %0d, expected 44", second_block_v);
    $display("All checks passed");
  end
endmodule
