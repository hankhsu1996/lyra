module Top;
  int eq_ne;
  int ne_ne;
  int lt_ne;
  int le_ne;
  int gt_ne;
  int ge_ne;
  int eq_eq;
  int le_eq;
  int ge_eq;
  initial begin
    int a;
    int b;
    a = 5; b = 10;
    eq_ne = (a == b);
    ne_ne = (a != b);
    lt_ne = (a < b);
    le_ne = (a <= b);
    gt_ne = (a > b);
    ge_ne = (a >= b);
    a = 5; b = 5;
    eq_eq = (a == b);
    le_eq = (a <= b);
    ge_eq = (a >= b);
  end
endmodule
