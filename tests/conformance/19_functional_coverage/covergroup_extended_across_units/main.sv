// A covergroup a class of a package embeds may be extended by a class of
// another unit that extends that class: the package's `new` makes the derived
// instance, sampling it samples the base's items and the derived ones, and a
// coverpoint the derived covergroup declares again replaces the base's
// (LRM 19.4.1, 26.3).
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

  initial begin
    static Derived d = new;
    static Base as_base = d;
    d.a = 1; d.cg.sample();
    // A handle of the base class samples the same, derived, instance.
    as_base.a = 0; as_base.cg.sample();
    inst_cov = d.cg.get_inst_coverage();
    pa_cov = d.cg.pa.get_inst_coverage();
    pq_cov = as_base.cg.pq.get_inst_coverage();
  end

  final begin
    // The derived `pa` saw 1 of its two values and `pq` saw both of its; the
    // base's `pa` does not count.
    if (pa_cov != 50.0) $fatal(1, "pa's coverage was %0f, expected 50", pa_cov);
    if (pq_cov != 100.0) $fatal(1, "pq's coverage was %0f, expected 100", pq_cov);
    if (inst_cov != 75.0) $fatal(1, "the instance's coverage was %0f, expected 75", inst_cov);
    $display("All checks passed");
  end
endmodule
