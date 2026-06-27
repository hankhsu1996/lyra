module Top;
  typedef struct {
    int  a;
    byte b;
  } pair_t;

  typedef struct {
    pair_t inner;
    int    tag;
  } nest_t;

  typedef struct {
    int        a;
    logic [7:0] b;
  } lt_t;

  pair_t p1;
  pair_t p2;
  pair_t p3;
  nest_t n1;
  nest_t n2;
  nest_t n3;
  lt_t   x1;
  lt_t   x2;
  lt_t   x3;

  int eq_same;
  int eq_diff;
  int ne_same;
  int ne_diff;
  int ceq_same;
  int ceq_diff;
  int cne_same;
  int cne_diff;
  int ceq_nest;
  int ceq_nest_diff;
  int ceq_x;
  int ceq_x_diff;
  int sum;
  logic [3:0] slc;

  initial begin
    p1 = '{a: 5, b: 8'd6};
    p2 = '{a: 5, b: 8'd6};
    p3 = '{a: 5, b: 8'd7};

    eq_same = (p1 == p2);
    eq_diff = (p1 == p3);
    ne_same = (p1 != p2);
    ne_diff = (p1 != p3);
    ceq_same = (p1 === p2);
    ceq_diff = (p1 === p3);
    cne_same = (p1 !== p2);
    cne_diff = (p1 !== p3);

    n1 = '{inner: '{a: 7, b: 8'd8}, tag: 9};
    n2 = n1;
    n3 = '{inner: '{a: 7, b: 8'd8}, tag: 10};
    ceq_nest = (n1 === n2);
    ceq_nest_diff = (n1 === n3);

    x1 = '{a: 5, b: 8'bx};
    x2 = '{a: 5, b: 8'bx};
    x3 = '{a: 6, b: 8'bx};
    ceq_x = (x1 === x2);
    ceq_x_diff = (x1 === x3);

    sum = p1.a + 1;
    slc = p1.b[3:0];
  end
endmodule
