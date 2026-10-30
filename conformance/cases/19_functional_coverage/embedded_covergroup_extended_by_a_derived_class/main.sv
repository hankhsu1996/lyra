// A class extending one that embeds a covergroup may extend the covergroup
// too. The derived covergroup's instance replaces the base's, so the base's
// `new` makes it and every reference to the name, in either class, reaches it;
// a coverpoint it declares again stops the base's from counting, one it adds
// counts beside the base's, and an option it sets overrides the base's. The
// base and the derived covergroup are separate types (LRM 19.4.1).
module Top;
  class Base;
    int a, b;

    covergroup first;
      coverpoint a { bins one = {1}; }
    endgroup

    covergroup cg;
      option.weight = 10;
      pa: coverpoint a { bins lo = {[0:1]}; bins hi = {[2:3]}; }
      pb: coverpoint b { bins b[] = {[0:1]}; }
    endgroup

    function new();
      first = new;
      cg = new;
    endfunction

    function void tick(int to_a, int to_b);
      a = to_a;
      b = to_b;
      cg.sample();
    endfunction
  endclass

  class Derived extends Base;
    int c;

    covergroup extends cg;
      option.weight = 2;
      pa: coverpoint a { bins zero = {0}; bins one = {1}; }
      pc: coverpoint c { bins c[] = {[0:3]}; }
    endgroup

    function new();
      super.new();
    endfunction
  endclass

  real inst_cov = -1, pa_cov = -1, pb_cov = -1, pc_cov = -1;
  real derived_type_cov = -1, base_type_cov = -1;
  int weight = -1;
  bit first_made = 0;

  initial begin
    static Derived d = new;
    static Base plain = new;
    // The base's method samples the derived instance its `new` made.
    d.c = 0; d.tick(0, 0);
    d.c = 1; d.tick(0, 1);
    plain.tick(3, 0);
    inst_cov = d.cg.get_inst_coverage();
    pa_cov = d.cg.pa.get_inst_coverage();
    pb_cov = d.cg.pb.get_inst_coverage();
    pc_cov = d.cg.pc.get_inst_coverage();
    weight = d.cg.option.weight;
    derived_type_cov = d.cg.get_coverage();
    base_type_cov = plain.cg.get_coverage();
    first_made = d.first != null;
  end

  function automatic bit near(real got, real want);
    return got - want < 0.01 && want - got < 0.01;
  endfunction

  final begin
    // The derived `pa` saw 0 and never 1; `pb` saw 0 and 1; `pc` saw 0 and 1
    // of four; the base's `pa` no longer counts.
    if (!near(pa_cov, 50.0)) $fatal(1, "pa's coverage was %0f, expected 50", pa_cov);
    if (!near(pb_cov, 100.0)) $fatal(1, "pb's coverage was %0f, expected 100", pb_cov);
    if (!near(pc_cov, 50.0)) $fatal(1, "pc's coverage was %0f, expected 50", pc_cov);
    if (!near(inst_cov, 200.0 / 3.0))
      $fatal(1, "the instance's coverage was %0f, expected 66.67", inst_cov);
    if (weight != 2) $fatal(1, "the weight was %0d, expected the derived 2", weight);
    // Only the derived instance is of the derived type, and only the plain
    // object's of the base type: `pa` saw 3 (one of two bins), `pb` saw 0.
    if (!near(derived_type_cov, 200.0 / 3.0))
      $fatal(1, "the derived type's coverage was %0f, expected 66.67", derived_type_cov);
    if (!near(base_type_cov, 50.0))
      $fatal(1, "the base type's coverage was %0f, expected 50", base_type_cov);
    if (!first_made) $fatal(1, "the covergroup nothing extends was not made");
    $display("All checks passed");
  end
endmodule
