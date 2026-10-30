// A bin construct's `iff` guards every bin the construct makes: while it is
// false, a sample counts in none of them (LRM 19.5.1), for value bins after an
// array of them, for transition bins (LRM 19.5.2), and for a cross's bins
// (LRM 19.6.1). A construct with no guard counts as usual beside it.
module Top;
  bit g;
  int v;
  int a;
  int b;

  covergroup values;
    coverpoint v {
      bins arr[2] = {[0:3]};
      bins guarded = {[8:9]} iff (g);
      bins other = {12};
    }
  endgroup

  covergroup transitions;
    coverpoint v {
      bins up = (1 => 2) iff (g);
      bins down = (2 => 1);
    }
  endgroup

  covergroup crossed;
    pa: coverpoint a { bins x = {1}; bins y = {2}; }
    pb: coverpoint b { bins z = {1}; }
    c: cross pa, pb {
      bins guarded = binsof(pa.x) iff (g);
      bins open = binsof(pa.y);
    }
  endgroup

  real value_cov;
  real transition_cov;
  real cross_cov;

  initial begin
    static values vg = new;
    static transitions tg = new;
    static crossed cg = new;
    g = 0;
    v = 8;
    vg.sample();
    v = 12;
    vg.sample();
    v = 1;
    tg.sample();
    v = 2;
    tg.sample();
    v = 1;
    tg.sample();
    a = 1;
    b = 1;
    cg.sample();
    value_cov = vg.get_inst_coverage();
    transition_cov = tg.v.get_inst_coverage();
    cross_cov = cg.c.get_inst_coverage();
  end

  final begin
    if (value_cov != 25.0)
      $fatal(1, "the value bins covered %0f, expected 25: one of four", value_cov);
    if (transition_cov != 50.0)
      $fatal(1, "the transition bins covered %0f, expected 50: one of two",
             transition_cov);
    if (cross_cov != 0.0)
      $fatal(1, "the cross bins covered %0f, expected 0", cross_cov);
    $display("All checks passed");
  end
endmodule
