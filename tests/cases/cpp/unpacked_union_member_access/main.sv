module Top;
  typedef union {
    int        i;
    bit [15:0] h;
  } two_t;

  typedef union {
    byte       b;
    int        w;
    bit [15:0] h;
  } three_t;

  typedef struct {
    int x;
    int y;
  } pair_t;

  typedef union {
    pair_t     p;
    bit [15:0] h;
  } nested_t;

  two_t    a;
  two_t    d;
  two_t    c;
  two_t    n;
  three_t  t;
  nested_t nu;
  int      def_i;
  int      a_rt;
  int      t_rt;
  int      t_ovr;
  int      t_h;
  int      c_rt;
  int      n_rt;
  int      nu_x;
  int      nu_y;
  int      nu_hbit;

  initial begin
    def_i = d.i;

    a.i = 42;
    a_rt = a.i;

    t.w = 32'h0AABBCCD;
    t_rt = t.w;

    t.b = 8'h7F;
    t_ovr = t.b;

    t.h = 16'hBEEF;
    t_h = t.h;

    c.i = 10;
    c.i += 5;
    c_rt = c.i;

    n.i = 1;
    n.i <= 99;
    #1;
    n_rt = n.i;

    nu.p.x = 11;
    nu.p.y = 22;
    nu_x = nu.p.x;
    nu_y = nu.p.y;

    nu.h = 16'h0000;
    nu.h[4] = 1'b1;
    nu_hbit = nu.h;
  end
endmodule
