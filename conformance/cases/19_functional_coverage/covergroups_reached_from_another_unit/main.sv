// A covergroup may be declared in a package or in the compilation-unit scope
// as well as in a module or a class (LRM 19.3, 26.2), and is then a type other
// scopes name; an instance a module holds is reached from another module by a
// hierarchical name (LRM 23.6). Each is the same covergroup wherever it is
// reached: its `sample` counts what it is given, its options are the ones the
// instance holds, and its coverage answers the same from every scope. One made
// in another module with `ref` arguments reads what they name there, and is
// sampled on its clocking event over one of them (LRM 19.3).
covergroup unit_cg with function sample(int v);
  coverpoint v { bins b[] = {[0:3]}; }
endgroup

package P;
  covergroup package_cg with function sample(int v);
    coverpoint v { bins b[] = {[0:3]}; }
  endgroup

  covergroup watching (ref bit clk, ref int x) @(posedge clk);
    coverpoint x { bins b[] = {[0:3]}; }
  endgroup

  class Holder;
    int x;
    covergroup embedded;
      coverpoint x { bins b[] = {[0:3]}; }
    endgroup
    function new();
      embedded = new;
    endfunction
  endclass
endpackage

module Leaf;
  int v;
  covergroup leaf_cg;
    coverpoint v { bins b[] = {[0:3]}; }
  endgroup
  leaf_cg g = new;
endmodule

module Top;
  Leaf leaf ();
  unit_cg u = new;
  P::package_cg p = new;
  P::Holder h = new;
  bit clk;
  int x;
  P::watching w = new(clk, x);

  initial begin
    leaf.v = 1;
    leaf.g.sample();
    leaf.g.option.weight = 3;
    u.sample(0);
    u.sample(1);
    p.sample(2);
    h.x = 3;
    h.embedded.sample();
    w.option.weight = 2;
    x = 1;
    #1 clk = 1;
    #1 clk = 0;
    x = 2;
    #1 clk = 1;
  end

  final begin
    if (leaf.g.get_inst_coverage() != 25.0)
      $fatal(1, "the instance reached by name covered %0f, expected 25",
             leaf.g.get_inst_coverage());
    if (leaf.g.option.weight != 3)
      $fatal(1, "the instance reached by name has weight %0d, expected 3",
             leaf.g.option.weight);
    if (u.get_inst_coverage() != 50.0)
      $fatal(1, "the compilation-unit covergroup covered %0f, expected 50",
             u.get_inst_coverage());
    if (p.get_inst_coverage() != 25.0)
      $fatal(1, "the package covergroup covered %0f, expected 25",
             p.get_inst_coverage());
    if (P::package_cg::get_coverage() != 25.0)
      $fatal(1, "the package covergroup's type covered %0f, expected 25",
             P::package_cg::get_coverage());
    if (h.embedded.get_inst_coverage() != 25.0)
      $fatal(1, "the covergroup a package class embeds covered %0f, expected 25",
             h.embedded.get_inst_coverage());
    if (w.get_inst_coverage() != 50.0)
      $fatal(1, "the covergroup reading its ref arguments covered %0f, expected 50",
             w.get_inst_coverage());
    if (w.option.weight != 2)
      $fatal(1, "the covergroup reading its ref arguments has weight %0d, expected 2",
             w.option.weight);
    $display("All checks passed");
  end
endmodule
