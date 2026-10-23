// A cross bin is the tuples its select expression names: `binsof` with
// `intersect`, negated and combined (LRM 19.6.1); a `with` filter keeps the
// tuples enough of whose value tuples satisfy it, one by default and every one
// under `matches $` (LRM 19.6.1.2); ignored tuples count toward nothing, and
// automatic tuples are kept beside user bins unless the option says otherwise
// (LRM 19.6).
module Top;
  bit [7:0] v_a, v_b;
  bit [2:0] p, q;

  covergroup selects;
    a: coverpoint v_a {
      bins a1 = {[0:63]};
      bins a2 = {[64:127]};
      bins a3 = {[128:191]};
      bins a4 = {[192:255]};
    }
    b: coverpoint v_b {
      bins b1 = {0};
      bins b2 = {[1:84]};
      bins b3 = {[85:169]};
      bins b4 = {[170:255]};
    }
    c: cross a, b {
      bins c1 = !binsof(a) intersect {[100:200]};
      bins c2 = binsof(a.a2) || binsof(b.b2);
      bins c3 = binsof(a.a1) && binsof(b.b4);
      ignore_bins ig = binsof(a.a4) && binsof(b.b1);
    }
  endgroup

  covergroup filters;
    cp: coverpoint p { bins lo[] = {[0:3]}; bins hi = {[4:7]}; }
    cq: coverpoint q { bins lo[] = {[0:3]}; bins hi = {[4:7]}; }
    some: cross cp, cq { option.cross_retain_auto_bins = 0; bins s = some with (cp == cq); }
    every: cross cp, cq {
      option.cross_retain_auto_bins = 0;
      bins s = every with (cp == cq) matches $;
    }
    negated: cross cp, cq {
      option.cross_retain_auto_bins = 0;
      bins s = !binsof(cq.lo) && binsof(cp.hi);
    }
  endgroup

  int covered = -1, total = -1;
  real c_cov, some_cov, every_cov, negated_cov;

  initial begin
    static selects g = new;
    static filters f = new;
    v_a = 10; v_b = 200; g.sample();
    v_a = 70; v_b = 0; g.sample();
    v_a = 130; v_b = 90; g.sample();
    v_a = 200; v_b = 0; g.sample();
    c_cov = g.c.get_inst_coverage(covered, total);
    // <hi, hi> has a value tuple with p == q, but not every one of its 16.
    p = 5; q = 6; f.sample();
    some_cov = f.some.get_inst_coverage();
    every_cov = f.every.get_inst_coverage();
    negated_cov = f.negated.get_inst_coverage();
  end

  final begin
    if (covered !== 4 || total !== 8)
      $fatal(1, "%0d of %0d cross bins were covered, expected 4 of 8", covered, total);
    if (c_cov != 50.0) $fatal(1, "the cross's coverage was %0f, expected 50", c_cov);
    if (some_cov != 100.0) $fatal(1, "the filtered bin's coverage was %0f, expected 100", some_cov);
    if (every_cov != 0.0) $fatal(1, "the 'matches $' bin's coverage was %0f, expected 0", every_cov);
    if (negated_cov != 100.0)
      $fatal(1, "the negated bin's coverage was %0f, expected 100", negated_cov);
    $display("All checks passed");
  end
endmodule
