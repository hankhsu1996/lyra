module Top;
  typedef struct {
    int  a;
    byte b;
    int  c;
  } tri_t;

  typedef struct {
    tri_t inner;
    int   tag;
  } nest_t;

  typedef struct {
    int a;
    int b;
  } dup_t;

  tri_t  pos;
  tri_t  nm;
  tri_t  tk;
  tri_t  df;
  nest_t ns;
  dup_t  rp;
  nest_t pw;
  tri_t  cmp;

  int pos_a;
  int pos_b;
  int pos_c;
  int nm_a;
  int nm_b;
  int nm_c;
  int tk_a;
  int tk_b;
  int tk_c;
  int df_a;
  int df_b;
  int df_c;
  int ns_ia;
  int ns_ib;
  int ns_ic;
  int ns_tag;
  int rp_a;
  int rp_b;
  int pw_ia;
  int pw_tag;
  int eq_same;
  int eq_diff;

  initial begin
    pos = '{1, 8'd2, 3};
    pos_a = pos.a;
    pos_b = pos.b;
    pos_c = pos.c;

    nm = '{c: 30, a: 10, b: 8'd20};
    nm_a = nm.a;
    nm_b = nm.b;
    nm_c = nm.c;

    tk = '{int: 7, byte: 8'd8};
    tk_a = tk.a;
    tk_b = tk.b;
    tk_c = tk.c;

    df = '{default: 5};
    df_a = df.a;
    df_b = df.b;
    df_c = df.c;

    ns = '{inner: '{a: 100, b: 8'd101, c: 102}, tag: 9};
    ns_ia = ns.inner.a;
    ns_ib = ns.inner.b;
    ns_ic = ns.inner.c;
    ns_tag = ns.tag;

    rp = '{2{7}};
    rp_a = rp.a;
    rp_b = rp.b;

    pw.tag = 1;
    pw.inner = '{a: 55, b: 8'd0, c: 0};
    pw_ia = pw.inner.a;
    pw_tag = pw.tag;

    cmp = '{1, 8'd2, 3};
    eq_same = (cmp == tri_t'{1, 8'd2, 3});
    eq_diff = (cmp == tri_t'{9, 8'd2, 3});
  end
endmodule
