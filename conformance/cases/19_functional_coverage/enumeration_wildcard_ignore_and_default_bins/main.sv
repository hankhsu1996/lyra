// An enumeration's automatic bins are its values (LRM 19.5.3); a wildcard bin
// matches where its `?` bits do not care (LRM 19.5.4); an ignored value counts
// toward no bin (LRM 19.5.5); a default bin catches what no other bin does and
// counts toward no coverage (LRM 19.5); a real coverpoint's bins are intervals
// (LRM 19.5.1). Options are read and written as fields (LRM 19.7), and a
// stopped instance samples nothing (LRM 19.8).
module Top;
  typedef enum {RED, GREEN, BLUE} color_t;
  color_t c;
  bit [3:0] v;
  real r;

  covergroup cg;
    option.per_instance = 1;
    type_option.comment = "colors";
    col: coverpoint c;
    w: coverpoint v {
      wildcard bins top = {4'b11??};
      ignore_bins skip = {0};
      bins others[] = {[1:3]};
      bins rest = default;
    }
    rv: coverpoint r {
      bins near = {[0.0:1.0]};
      bins far = {[10.0:20.0]};
    }
  endgroup

  cg g = new;
  real col_cov, w_cov, rv_cov, all_cov, stopped_cov;
  int weight, type_weight;
  string comment;

  initial begin
    c = RED; v = 13; r = 0.5; g.sample();
    c = BLUE; v = 0; r = 15.0; g.sample();
    c = BLUE; v = 2; r = 5.0; g.sample();
    col_cov = g.col.get_inst_coverage();
    w_cov = g.w.get_inst_coverage();
    rv_cov = g.rv.get_inst_coverage();
    all_cov = g.get_inst_coverage();
    g.option.weight = 3;
    cg::type_option.weight = 2;
    weight = g.option.weight;
    type_weight = cg::type_option.weight;
    comment = cg::type_option.comment;
    g.stop();
    v = 1; g.sample();
    stopped_cov = g.w.get_inst_coverage();
  end

  final begin
    if (col_cov < 66.66 || col_cov > 66.67)
      $fatal(1, "col's coverage was %0f, expected 66.67", col_cov);
    // top (13) and others[2] of top and others[1..3].
    if (w_cov != 50.0) $fatal(1, "w's coverage was %0f, expected 50", w_cov);
    if (rv_cov != 100.0) $fatal(1, "rv's coverage was %0f, expected 100", rv_cov);
    if (all_cov < 72.22 || all_cov > 72.23)
      $fatal(1, "the instance's coverage was %0f, expected 72.22", all_cov);
    if (weight !== 3 || type_weight !== 2 || comment != "colors")
      $fatal(1, "the options read %0d, %0d and '%s'", weight, type_weight, comment);
    if (stopped_cov != 50.0)
      $fatal(1, "a stopped instance's coverage moved to %0f", stopped_cov);
    $display("All checks passed");
  end
endmodule
