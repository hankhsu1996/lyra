// A covergroup extending another holds what the other holds, and an item it
// declares under the name of an item of the same kind there replaces that
// item: the replaced one no longer counts toward the instance (LRM 19.4.1).
// The base covergroup is the whole lineage above, so a coverpoint replaces one
// declared two covergroups up as surely as one declared just above. An item
// of the other kind replaces nothing, so a coverpoint named like a cross adds
// to what counts, and a cross over a replaced coverpoint still counts.
module Top;
  class Base;
    int a;
    int b;
    covergroup cg;
      // Four bins, one of them hit by a == 0.
      c: coverpoint a { bins quarter[] = {[0:3]}; }
      // Two bins, one hit by b == 0.
      pb: coverpoint b { bins zero = {0}; bins one = {1}; }
      // Eight tuples, one hit.
      x: cross c, pb;
    endgroup

    function new();
      cg = new;
    endfunction
  endclass

  // Declares no item of its own.
  class Mid extends Base;
    covergroup extends cg;
      option.weight = 1;
    endgroup

    function new();
      super.new();
    endfunction
  endclass

  class Leaf extends Mid;
    covergroup extends cg;
      // Replaces Base's `c`, two covergroups up: one bin, hit.
      c: coverpoint a { bins zero = {0}; }
      // Named like Base's cross, and a coverpoint: replaces nothing.
      x: coverpoint b { bins zero = {0}; }
    endgroup

    function new();
      super.new();
    endfunction
  endclass

  function automatic bit near(real got, real want);
    return got > want - 0.01 && got < want + 0.01;
  endfunction

  initial begin
    static Leaf leaf = new;
    real inst;
    leaf.a = 0;
    leaf.b = 0;
    leaf.cg.sample();
    // pb 50, Base's x 12.5, Leaf's c 100, Leaf's x 100.
    inst = leaf.cg.get_inst_coverage();
    if (!near(inst, 65.625)) begin
      $fatal(1, "a lineage replacing by kind and name covered %0f, expected 65.625", inst);
    end
    if (!near(leaf.cg.c.get_inst_coverage(), 100.0)) begin
      $fatal(1, "the coverpoint replacing one two levels up was not the one named");
    end
    if (!near(leaf.cg.x.get_inst_coverage(), 100.0)) begin
      $fatal(1, "the coverpoint named like a cross was not the one named");
    end
    $display("All checks passed");
  end
endmodule
