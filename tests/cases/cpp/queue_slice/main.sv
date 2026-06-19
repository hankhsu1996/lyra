// LRM 7.10 `$` last index and LRM 7.10.1 queue slice. `$` denotes the last
// index (and `$-1` the one before). The slice `q[a:b]` clamps `a` up to 0 and
// `b` down to the last index, yields the empty queue when `a > b` or a bound is
// x/z, and `q = q[1:$]` is the pop-front idiom of LRM 7.10.4. The indexed forms
// `q[base+:w]` and `q[base-:w]` reduce to the same low / high pair and clamp the
// same way.
module Top;
  int q [$] = '{10, 20, 30, 40, 50};
  logic [31:0] bad;

  int last;
  int second_last;
  int s_const [$];
  int s_to_end [$];
  int s_drop_last [$];
  int s_empty_rev [$];
  int s_clamp_lo [$];
  int s_clamp_hi [$];
  int xz [$];
  int up [$];
  int down [$];
  int up_clamp [$];
  int down_clamp [$];
  int pf [$] = '{1, 2, 3, 4};

  initial begin
    last = q[$];
    second_last = q[$-1];
    s_const = q[1:3];
    s_to_end = q[1:$];
    s_drop_last = q[0:$-1];
    s_empty_rev = q[3:1];
    s_clamp_lo = q[-2:1];
    s_clamp_hi = q[3:99];
    bad = 32'hx;
    xz = q[0:bad];
    up = q[1 +: 3];
    down = q[3 -: 2];
    up_clamp = q[3 +: 5];
    down_clamp = q[1 -: 4];
    pf = pf[1:$];
    pf = pf[1:$];
  end
endmodule
