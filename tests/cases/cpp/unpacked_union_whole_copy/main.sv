module Top;
  typedef struct {
    int x;
    int y;
  } pair_t;

  typedef union {
    int        i;
    bit [15:0] h;
  } scalar_t;

  typedef union {
    pair_t     p;
    bit [15:0] h;
  } nested_t;

  scalar_t a;
  scalar_t b;
  nested_t na;
  nested_t nb;

  int copy_rt;
  int indep_rt;
  int arith_rt;
  int nested_x_rt;
  int nested_y_rt;
  int nested_indep_rt;

  initial begin
    a.i = 42;
    b = a;
    copy_rt = b.i;

    a.i = 7;
    indep_rt = b.i;

    arith_rt = a.i * 2 + (b.i > a.i ? 100 : 1);

    na.p.x = 11;
    na.p.y = 22;
    nb = na;
    nested_x_rt = nb.p.x;
    nested_y_rt = nb.p.y;

    na.p.x = 99;
    nested_indep_rt = nb.p.x;
  end
endmodule
