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

  two_t   a;
  two_t   d;
  three_t t;
  int     def_i;
  int     a_rt;
  int     t_rt;
  int     t_ovr;
  int     t_h;

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
  end
endmodule
