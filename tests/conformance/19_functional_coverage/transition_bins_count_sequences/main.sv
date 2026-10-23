// A transition bin counts each sequence of sampled values it names, ranges of
// values and repetitions included: consecutive `[*]`, goto `[->]` and
// nonconsecutive `[=]` (LRM 19.5.2); a `default sequence` bin catches every
// other sequence and counts toward no coverage. A repetition written with one
// count repeats exactly that many times, the count evaluated once.
module Top;
  bit [4:1] v_a;
  int v;
  int counted = 0;

  function automatic int three();
    counted++;
    return 3;
  endfunction

  covergroup ranges;
    coverpoint v_a {
      bins sa = (4 => 5 => 6), ([7:9], 10 => 11, 12);
      bins sb[] = (4 => 5 => 6), ([7:9], 10 => 11, 12);
      bins sc = (12 => 3 [-> 1]);
      bins allother = default sequence;
    }
  endgroup

  covergroup repeats;
    coverpoint v {
      bins b2 = (2 [-> 3:5]);
      bins b3 = (3 [-> 3:5]);
      bins b5 = (5 [* three()]);
      bins b6 = (1 => 3 [-> 4:6] => 1);
      bins b7 = (1 => 2 [= 3:6] => 5);
    }
  endgroup

  int seq[] = '{1, 4, 3, 2, 3, 3, 2, 2, 3, 2, 3, 1, 5, 5, 5, 5, 5, 5};
  int g_covered = -1, g_total = -1, s_covered = -1, s_total = -1;

  initial begin
    static ranges g = new;
    static repeats s = new;
    real ignored;
    v_a = 4; g.sample();
    v_a = 5; g.sample();
    v_a = 7; g.sample();
    v_a = 11; g.sample();
    v_a = 8; g.sample();
    v_a = 12; g.sample();
    v_a = 2; g.sample();
    v_a = 2; g.sample();
    v_a = 3; g.sample();
    ignored = g.get_inst_coverage(g_covered, g_total);
    foreach (seq[i]) begin
      v = seq[i];
      s.sample();
    end
    ignored = s.get_inst_coverage(s_covered, s_total);
  end

  final begin
    if (g_covered !== 4 || g_total !== 11)
      $fatal(1, "%0d of %0d transition bins covered, expected 4 of 11", g_covered, g_total);
    if (counted !== 1)
      $fatal(1, "the repetition's count was evaluated %0d times, expected once", counted);
    if (s_covered !== 5 || s_total !== 5)
      $fatal(1, "%0d of %0d repetition bins covered, expected 5 of 5", s_covered, s_total);
    $display("All checks passed");
  end
endmodule
