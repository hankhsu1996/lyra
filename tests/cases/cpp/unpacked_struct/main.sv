module Top;
  typedef struct {
    int a;
    byte b;
  } pair_t;

  pair_t s;
  pair_t cp;
  pair_t other;

  int def_a;
  int def_b;
  int r_a;
  int r_b;
  int cp_a;
  int cp_b;
  int eq_same;
  int eq_diff;

  initial begin
    def_a = s.a;
    def_b = s.b;

    s.a = 42;
    s.b = 8'h7F;
    r_a = s.a;
    r_b = s.b;

    cp = s;
    s.a = 99;
    cp_a = cp.a;
    cp_b = cp.b;

    other.a = 99;
    other.b = 8'h7F;
    eq_same = (s == other);
    eq_diff = (cp == other);
  end
endmodule
