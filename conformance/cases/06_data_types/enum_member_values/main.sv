// An enumerated name given no value takes one more than the name declared
// before it, and the first name, given no value, takes zero. An explicit value
// overrides that count wherever it appears, and the names after it continue
// from the value it set rather than from the position they occupy (LRM 6.19).
module Top;
  typedef enum {A, B, C, D} seq_t;
  typedef enum {P = 1, Q = 5, R = 10} set_t;
  typedef enum {S = 5, T, U = 20, V} mixed_t;

  seq_t sv;
  set_t pv;
  mixed_t mv;

  int seq_a = -1;
  int seq_b;
  int seq_c;
  int seq_d;
  int set_p;
  int set_q;
  int set_r;
  int mix_s;
  int mix_t;
  int mix_u;
  int mix_v;

  initial begin
    sv = A;
    seq_a = sv;
    sv = B;
    seq_b = sv;
    sv = C;
    seq_c = sv;
    sv = D;
    seq_d = sv;

    pv = P;
    set_p = pv;
    pv = Q;
    set_q = pv;
    pv = R;
    set_r = pv;

    mv = S;
    mix_s = mv;
    mv = T;
    mix_t = mv;
    mv = U;
    mix_u = mv;
    mv = V;
    mix_v = mv;
  end

  final begin
    if (seq_a !== 0) $fatal(1, "seq_a was %0d, expected 0", seq_a);
    if (seq_b !== 1) $fatal(1, "seq_b was %0d, expected 1", seq_b);
    if (seq_c !== 2) $fatal(1, "seq_c was %0d, expected 2", seq_c);
    if (seq_d !== 3) $fatal(1, "seq_d was %0d, expected 3", seq_d);
    if (set_p !== 1) $fatal(1, "set_p was %0d, expected 1", set_p);
    if (set_q !== 5) $fatal(1, "set_q was %0d, expected 5", set_q);
    if (set_r !== 10) $fatal(1, "set_r was %0d, expected 10", set_r);
    if (mix_s !== 5) $fatal(1, "mix_s was %0d, expected 5", mix_s);
    if (mix_t !== 6) $fatal(1, "mix_t was %0d, expected 6", mix_t);
    if (mix_u !== 20) $fatal(1, "mix_u was %0d, expected 20", mix_u);
    if (mix_v !== 21) $fatal(1, "mix_v was %0d, expected 21", mix_v);
    $display("All checks passed");
  end
endmodule
