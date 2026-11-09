// A coverpoint or a cross is named by its label, or, written with none over one
// variable, by the variable's name; one over anything else has a name only for
// its report, and a variable a cross names with no coverpoint for it is a
// coverpoint of its own (LRM 19.5, 19.6). An item's `get_coverage` and its
// `type_option` are the type's, over every instance (LRM 19.8, 19.11.3), and
// a cross bin selecting `binsof` a bin of a coverpoint selects every bin that
// one bin construct made, an array's elements and a transition's included,
// whatever other bin shares the start of its name (LRM 19.6.1).
module Top;
  int v;
  int w;
  bit u;

  covergroup cg;
    // Three bins: fix[0] = {0,1}, fix[1] = {2,3}, and the transition t.
    a: coverpoint v {
      bins fix[2] = {[0:3]};
      bins t = (0 => 1);
      bins fixed = {9};
    }
    // Named `w`, by its variable.
    coverpoint w { bins lo = {0}; bins hi = {1}; }
    // Named only for its report.
    coverpoint v + w { bins zero = {0}; }
    s: cross a, w {
      option.cross_retain_auto_bins = 0;
      bins f = binsof(a.fix) && binsof(w.lo);
      bins tr = binsof(a.t);
    }
    // `u` names no coverpoint, so the cross declares one for it.
    y: cross a, u;
  endgroup

  covergroup merged;
    type_option.merge_instances = 1;
    p: coverpoint v { bins zero = {0}; bins one = {1}; }
    q: coverpoint v { bins zero = {0}; }
  endgroup

  function automatic bit near(real got, real want);
    return got > want - 0.01 && got < want + 0.01;
  endfunction

  initial begin
    static cg first = new;
    static cg second = new;
    static merged m = new;
    v = 0;
    w = 0;
    u = 0;
    first.sample();
    // f's tuple (fix[0], lo) is hit; tr is not yet.
    if (!near(first.s.get_inst_coverage(), 50.0)) begin
      $fatal(1, "the cross covered %0f after one sample, expected 50",
             first.s.get_inst_coverage());
    end
    // Eight tuples of `a` and the implicit `u`, one hit.
    if (!near(first.y.get_inst_coverage(), 12.5)) begin
      $fatal(1, "the cross over an implicit coverpoint covered %0f, expected 12.5",
             first.y.get_inst_coverage());
    end
    v = 1;
    w = 1;
    first.sample();
    // 0 => 1 completed t, so tr is hit too.
    if (!near(first.s.get_inst_coverage(), 100.0)) begin
      $fatal(1, "the cross covered %0f after the transition, expected 100",
             first.s.get_inst_coverage());
    end
    v = 2;
    w = 0;
    second.sample();
    // `a`: first holds fix[0] and t of four bins, second fix[1]; `w`: first
    // both, second lo.
    if (!near(cg::a::get_coverage(), (50.0 + 25.0) / 2)) begin
      $fatal(1, "a's type coverage was %0f, expected 37.5", cg::a::get_coverage());
    end
    if (!near(cg::w::get_coverage(), (100.0 + 50.0) / 2)) begin
      $fatal(1, "w's type coverage was %0f, expected 75", cg::w::get_coverage());
    end
    cg::a::type_option.weight = 4;
    cg::s::type_option.weight = 2;
    if (cg::a::type_option.weight != 4 || cg::w::type_option.weight != 1 ||
        cg::s::type_option.weight != 2 || cg::y::type_option.weight != 1) begin
      $fatal(1, "an item's type option was not its own");
    end
    v = 0;
    m.sample();
    // p 50 and q 100, alike weighted, then p's weight taken away.
    if (!near(merged::get_coverage(), 75.0)) begin
      $fatal(1, "the merged type covered %0f, expected 75", merged::get_coverage());
    end
    merged::p::type_option.weight = 0;
    if (!near(merged::get_coverage(), 100.0)) begin
      $fatal(1, "the merged type covered %0f with p weighed nothing, expected 100",
             merged::get_coverage());
    end
    $display("All checks passed");
  end
endmodule
