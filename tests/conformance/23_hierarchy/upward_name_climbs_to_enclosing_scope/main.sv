// When the head of a dotted name is not visible where the reference is
// written, it is looked for in the scope the reference's own module was
// instantiated in and then upward through the enclosing scopes; once found,
// the rest of the name is resolved downward from there (LRM 23.8). The tail
// that follows the head is an ordinary hierarchical path, so it may name a
// variable on the head itself, descend through further instances, select an
// element of an instance array, or enter a named generate block, and the
// reference may be written as well as read. Nothing about the climb depends on
// how deep the reference sits or on whether it is written in the module body
// or inside a generate block, since generate scopes are among the enclosing
// scopes the search walks out through.
module Sib;
  int y;
endmodule

module Deep;
  int z;
endmodule

module MidH;
  Deep deep();
endmodule

module Bank;
  int y;
endmodule

module Leaf;
  int plain;
  int selected;
  int to_sib;
  int to_deep;
  int to_bank;
  int to_gen;

  always_comb plain = Top.g;
  always_comb selected = Top.g[3:0];
  always_comb to_sib = Top.sib.y;
  always_comb to_deep = Top.mid.deep.z;
  always_comb to_bank = Top.bank[2].y;
  always_comb to_gen = Top.row[1].w;

  if (1) begin : cond
    int from_cond;
    always_comb from_cond = Top.g;
  end

  for (genvar i = 0; i < 2; i = i + 1) begin : loop
    int from_loop;
    always_comb from_loop = Top.g;
  end
endmodule

module Mid;
  Leaf leaf();
endmodule

module Writer;
  initial Top.written = 99;

  if (1) begin : gw
    initial Top.written_in_generate = 77;
  end
endmodule

module Top;
  int g;
  int written;
  int written_in_generate;

  Sib sib();
  MidH mid();
  Bank bank [3] ();
  Leaf direct();
  Mid nested();
  Writer wr();

  for (genvar i = 0; i < 2; i = i + 1) begin : row
    int w = (i + 1) * 40;
  end

  initial begin
    g = 37;
    sib.y = 11;
    mid.deep.z = 22;
    bank[0].y = 44;
    bank[1].y = 55;
    bank[2].y = 33;
  end

  final begin
    if (direct.plain !== 37)
      $fatal(1, "direct.plain was %0d, expected 37", direct.plain);
    if (nested.leaf.plain !== 37)
      $fatal(1, "nested.leaf.plain was %0d, expected 37", nested.leaf.plain);
    if (direct.selected !== 5)
      $fatal(1, "direct.selected was %0d, expected 5", direct.selected);
    if (direct.to_sib !== 11)
      $fatal(1, "direct.to_sib was %0d, expected 11", direct.to_sib);
    if (direct.to_deep !== 22)
      $fatal(1, "direct.to_deep was %0d, expected 22", direct.to_deep);
    if (direct.to_bank !== 33)
      $fatal(1, "direct.to_bank was %0d, expected 33", direct.to_bank);
    if (direct.to_gen !== 80)
      $fatal(1, "direct.to_gen was %0d, expected 80", direct.to_gen);
    if (direct.cond.from_cond !== 37)
      $fatal(1, "direct.cond.from_cond was %0d, expected 37",
             direct.cond.from_cond);
    if (direct.loop[0].from_loop !== 37)
      $fatal(1, "direct.loop[0].from_loop was %0d, expected 37",
             direct.loop[0].from_loop);
    if (direct.loop[1].from_loop !== 37)
      $fatal(1, "direct.loop[1].from_loop was %0d, expected 37",
             direct.loop[1].from_loop);
    if (written !== 99) $fatal(1, "written was %0d, expected 99", written);
    if (written_in_generate !== 77)
      $fatal(1, "written_in_generate was %0d, expected 77",
             written_in_generate);
    $display("All checks passed");
  end
endmodule
