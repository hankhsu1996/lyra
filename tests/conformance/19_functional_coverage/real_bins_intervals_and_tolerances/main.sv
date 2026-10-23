// A real coverpoint's bins hold values and ranges; a range may be an absolute
// or a relative tolerance around a value, or open at either end with `$`
// (LRM 11.4.13, 19.5.1).
module Top;
  real r;

  covergroup cr;
    cp: coverpoint r {
      bins t = {[2.0 +/- 0.5]};
      bins p = {[10.0 +%- 10.0]};
      bins up = {[100.0:$]};
      bins down = {[$:-5.0]};
    }
  endgroup

  int hit_covered = -1, hit_total = -1, miss_covered = -1, miss_total = -1;

  initial begin
    static cr hit = new;
    static cr miss = new;
    real ignored;
    r = 2.4; hit.sample();
    r = 10.9; hit.sample();
    r = 1.0e9; hit.sample();
    r = -7.0; hit.sample();
    r = 2.6; miss.sample();
    r = 11.1; miss.sample();
    r = 99.0; miss.sample();
    r = -4.9; miss.sample();
    ignored = hit.cp.get_inst_coverage(hit_covered, hit_total);
    ignored = miss.cp.get_inst_coverage(miss_covered, miss_total);
  end

  final begin
    if (hit_covered !== 4 || hit_total !== 4)
      $fatal(1, "%0d of %0d bins covered by values inside, expected 4 of 4", hit_covered,
             hit_total);
    if (miss_covered !== 0 || miss_total !== 4)
      $fatal(1, "%0d of %0d bins covered by values outside, expected 0 of 4", miss_covered,
             miss_total);
    $display("All checks passed");
  end
endmodule
