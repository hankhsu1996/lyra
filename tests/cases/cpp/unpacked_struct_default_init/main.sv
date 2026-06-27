module Top;
  typedef struct {
    int  x;
    byte y = 8'd5;
  } inner_t;

  typedef struct {
    int         a = 42;
    byte        b;
    logic [3:0] g = 4'hC;
    inner_t     nested;
  } pair_t;

  // A member whose own declared default is itself an aggregate literal: a
  // struct-typed member and an array-typed member (LRM 7.2.2). The explicit
  // member default overrides the member type's own recursive default.
  typedef struct {
    inner_t agg_n = '{x: 11, y: 8'd12};
    int     agg_arr[2] = '{20, 21};
  } aggdef_t;

  pair_t s;
  pair_t s2 = '{a: 1, b: 8'd2, g: 4'h3, nested: '{x: 7, y: 8'd8}};
  pair_t arr[2];
  aggdef_t ad;

  int d_a;
  int d_b;
  int d_g;
  int d_nx;
  int d_ny;
  int e_a;
  int e_ny;
  int arr_a;
  int arr_ny;
  int loc_a;
  int loc_ny;
  int ad_nx;
  int ad_ny;
  int ad_a0;
  int ad_a1;

  initial begin
    pair_t loc;
    loc_a = loc.a;
    loc_ny = loc.nested.y;

    d_a = s.a;
    d_b = s.b;
    d_g = s.g;
    d_nx = s.nested.x;
    d_ny = s.nested.y;

    e_a = s2.a;
    e_ny = s2.nested.y;

    arr_a = arr[1].a;
    arr_ny = arr[0].nested.y;

    ad_nx = ad.agg_n.x;
    ad_ny = ad.agg_n.y;
    ad_a0 = ad.agg_arr[0];
    ad_a1 = ad.agg_arr[1];
  end
endmodule
