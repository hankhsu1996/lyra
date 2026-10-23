// A bin's values may be a queue and a covergroup's arguments, read as the
// instance is made (LRM 19.3, 19.5.1.2); `auto_bin_max` divides a coverpoint
// with no bins into that many automatic bins (LRM 19.5.3); a cross with no
// bins covers every pair of its coverpoints' bins (LRM 19.6); and the
// covergroup's coverage is the average of its items' (LRM 19.11).
module Top;
  int list_q[$] = '{1, 3, 5};
  bit [3:0] v;

  covergroup listed;
    cp: coverpoint v { bins vb[] = list_q; }
  endgroup

  covergroup ranged(int lo, int hi);
    a: coverpoint v { bins in_range = {[lo:hi]}; bins rest = {[0:$]}; }
    b: coverpoint v { option.auto_bin_max = 4; }
    x: cross a, b;
  endgroup

  real listed_cov, type_cov, ranged_cov, a_cov, b_cov, x_cov;

  initial begin
    static listed g = new;
    static ranged h = new(2, 5);
    v = 1; g.sample(); h.sample();
    v = 3; g.sample(); h.sample();
    v = 9; g.sample(); h.sample();
    listed_cov = g.get_inst_coverage();
    type_cov = listed::get_coverage();
    ranged_cov = h.get_inst_coverage();
    a_cov = h.a.get_inst_coverage();
    b_cov = h.b.get_inst_coverage();
    x_cov = h.x.get_inst_coverage();
  end

  final begin
    // 1 and 3 of 1, 3, 5.
    if (listed_cov < 66.66 || listed_cov > 66.67)
      $fatal(1, "the listed bins' coverage was %0f, expected 66.67", listed_cov);
    if (type_cov != listed_cov)
      $fatal(1, "the type's coverage was %0f, expected the one instance's %0f", type_cov,
             listed_cov);
    if (a_cov != 100.0) $fatal(1, "a's coverage was %0f, expected 100", a_cov);
    // 1 and 3 fall in [0:3], 9 in [8:11].
    if (b_cov != 50.0) $fatal(1, "b's coverage was %0f, expected 50", b_cov);
    // <rest, [0:3]>, <in_range, [0:3]> and <rest, [8:11]> of 8 pairs.
    if (x_cov != 37.5) $fatal(1, "x's coverage was %0f, expected 37.5", x_cov);
    if (ranged_cov != 62.5) $fatal(1, "the instance's coverage was %0f, expected 62.5", ranged_cov);
    $display("All checks passed");
  end
endmodule
