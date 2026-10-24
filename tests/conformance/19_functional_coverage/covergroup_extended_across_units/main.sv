// A covergroup a class of a package embeds may be extended by a class of
// another unit that extends that class: the package's `new` makes the derived
// instance, sampling it samples the base's items and the derived ones, and a
// coverpoint the derived covergroup declares again replaces the base's
// (LRM 19.4.1, 26.3). So may it by a specialization of a generic class of the
// same package, whose lineage passes back through the package, with the
// package's own code sampling it, and a class of the package extending the
// specialization may extend the covergroup again (LRM 8.25).
package shapes;
  class Base;
    int a;
    covergroup cg (int low);
      pa: coverpoint a { bins lo = {[low:3]}; bins hi = {[4:7]}; }
      pq: coverpoint a { bins q[] = {[0:1]}; }
    endgroup
    function new(int low);
      cg = new(low);
    endfunction
  endclass

  class Spec #(type T = int) extends Base;
    T unused;
    covergroup extends cg;
      pa: coverpoint a { bins three = {3}; }
    endgroup
    function new();
      super.new(0);
    endfunction
  endclass

  class Leaf extends Spec #(int);
    covergroup extends cg;
      pa: coverpoint a { bins one = {1}; bins two = {2}; }
    endgroup
    function new();
      super.new();
    endfunction
  endclass

  task automatic SampleALeaf(output real pa);
    Leaf l = new;
    Base as_base = l;
    as_base.a = 1; as_base.cg.sample();
    pa = l.cg.pa.get_inst_coverage();
  endtask

  task automatic SampleASpec(output real inst, output real pa, output real pq);
    Spec #(int) s = new;
    Base as_base = s;
    s.a = 3; s.cg.sample();
    as_base.a = 1; as_base.cg.sample();
    inst = s.cg.get_inst_coverage();
    pa = s.cg.pa.get_inst_coverage();
    pq = as_base.cg.pq.get_inst_coverage();
  endtask
endpackage

module Top;
  import shapes::*;

  class Derived extends Base;
    covergroup extends cg;
      pa: coverpoint a { bins one = {1}; bins two = {2}; }
    endgroup
    function new();
      super.new(0);
    endfunction
  endclass

  real inst_cov = -1, pa_cov = -1, pq_cov = -1;
  real spec_inst_cov = -1, spec_pa_cov = -1, spec_pq_cov = -1;
  real leaf_pa_cov = -1;

  initial begin
    static Derived d = new;
    static Base as_base = d;
    d.a = 1; d.cg.sample();
    // A handle of the base class samples the same, derived, instance.
    as_base.a = 0; as_base.cg.sample();
    inst_cov = d.cg.get_inst_coverage();
    pa_cov = d.cg.pa.get_inst_coverage();
    pq_cov = as_base.cg.pq.get_inst_coverage();
    SampleASpec(spec_inst_cov, spec_pa_cov, spec_pq_cov);
    SampleALeaf(leaf_pa_cov);
  end

  final begin
    // The derived `pa` saw 1 of its two values and `pq` saw both of its; the
    // base's `pa` does not count.
    if (pa_cov != 50.0) $fatal(1, "pa's coverage was %0f, expected 50", pa_cov);
    if (pq_cov != 100.0) $fatal(1, "pq's coverage was %0f, expected 100", pq_cov);
    if (inst_cov != 75.0) $fatal(1, "the instance's coverage was %0f, expected 75", inst_cov);
    // The specialization's `pa` saw its one value and `pq` one of its two.
    if (spec_pa_cov != 100.0)
      $fatal(1, "the specialization's pa coverage was %0f, expected 100", spec_pa_cov);
    if (spec_pq_cov != 50.0)
      $fatal(1, "the specialization's pq coverage was %0f, expected 50", spec_pq_cov);
    if (spec_inst_cov != 75.0)
      $fatal(1, "the specialization's instance coverage was %0f, expected 75", spec_inst_cov);
    // The leaf's own `pa` saw one of its two values.
    if (leaf_pa_cov != 50.0)
      $fatal(1, "the leaf's pa coverage was %0f, expected 50", leaf_pa_cov);
    $display("All checks passed");
  end
endmodule
