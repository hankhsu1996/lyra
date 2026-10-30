// A coverpoint's bins each count the sampled values that fall in them, a bin
// declared with `[]` is one bin per value, and an instance's coverage is the
// share of its bins hit at least once (LRM 19.5.1, 19.8, 19.11.1).
module Top;
  bit [3:0] v;

  covergroup cg;
    cp: coverpoint v {
      bins low = {[0:3]};
      bins mid[] = {5, 6};
      bins high = {[12:$]};
    }
  endgroup

  real unsampled = -1.0;
  real after = -1.0;
  int covered = -1;
  int total = -1;

  initial begin
    static cg g = new;
    unsampled = g.get_inst_coverage();
    v = 1;
    g.sample();
    v = 6;
    g.sample();
    v = 9;
    g.sample();
    after = g.get_inst_coverage(covered, total);
  end

  final begin
    if (unsampled != 0.0) $fatal(1, "coverage before any sample was %0f, expected 0", unsampled);
    if (covered !== 2 || total !== 4)
      $fatal(1, "%0d of %0d bins were covered, expected 2 of 4", covered, total);
    if (after != 50.0) $fatal(1, "coverage was %0f, expected 50", after);
    $display("All checks passed");
  end
endmodule
