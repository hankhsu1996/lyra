// LRM 11.2.2 + 11.4.5 logical equality on dynamic arrays. Aggregate ==
// reduces element-wise comparisons through &&; LRM 11.2.2 requires equivalent
// type but is silent on runtime size mismatch -- industry convention (and
// Verilator) yields 0 on size mismatch and 1 on empty-vs-empty. X / Z in any
// element comparison propagates to 1'bx per LRM 11.4.5.
module Top;
  int a [] = '{10, 20, 30};
  int b [] = '{10, 20, 30};
  int shorter [] = '{10, 20};
  int diff_val [] = '{10, 20, 99};
  int empty1 [];
  int empty2 [];

  // 2D pure dynamic.
  int m [][] = '{'{1, 2, 3}, '{4, 5, 6}};
  int n [][] = '{'{1, 2, 3}, '{4, 5, 6}};
  int row_mismatch [][] = '{'{1, 2, 3}, '{4, 5}};
  int outer_short [][] = '{'{1, 2, 3}};

  // Mixed: fixed outer, dynamic inner.
  int fdo_p [2][] = '{'{1, 2}, '{3, 4, 5}};
  int fdo_q [2][] = '{'{1, 2}, '{3, 4, 5}};
  int fdo_r [2][] = '{'{1, 2}, '{3, 4}};

  // Mixed: dynamic outer, fixed inner.
  int dfo_x [][3] = '{'{1, 2, 3}, '{4, 5, 6}};
  int dfo_y [][3] = '{'{1, 2, 3}, '{4, 5, 6}};
  int dfo_z [][3] = '{'{1, 2, 3}};

  // 4-state element type for X / Z propagation.
  logic [3:0] u_with_x [] = '{4'b1010, 4'b10x0, 4'b1111};
  logic [3:0] u_no_x   [] = '{4'b1010, 4'b1010, 4'b1111};

  bit eq_1d, ne_1d;
  bit eq_size_mismatch, ne_size_mismatch;
  bit eq_diff_val, ne_diff_val;
  bit eq_empty_empty, ne_empty_empty;
  bit eq_empty_nonempty, ne_empty_nonempty;
  bit eq_2d, eq_2d_row_mismatch, eq_2d_outer_mismatch;
  bit eq_fixed_outer, eq_fixed_outer_inner_diff;
  bit eq_dyn_outer, eq_dyn_outer_size_diff;
  logic eq_with_x, ne_with_x;

  initial begin
    eq_1d = (a == b);
    ne_1d = (a != b);
    eq_size_mismatch = (a == shorter);
    ne_size_mismatch = (a != shorter);
    eq_diff_val = (a == diff_val);
    ne_diff_val = (a != diff_val);

    eq_empty_empty = (empty1 == empty2);
    ne_empty_empty = (empty1 != empty2);
    eq_empty_nonempty = (empty1 == a);
    ne_empty_nonempty = (empty1 != a);

    eq_2d = (m == n);
    eq_2d_row_mismatch = (m == row_mismatch);
    eq_2d_outer_mismatch = (m == outer_short);

    eq_fixed_outer = (fdo_p == fdo_q);
    eq_fixed_outer_inner_diff = (fdo_p == fdo_r);
    eq_dyn_outer = (dfo_x == dfo_y);
    eq_dyn_outer_size_diff = (dfo_x == dfo_z);

    eq_with_x = (u_with_x == u_no_x);
    ne_with_x = (u_with_x != u_no_x);
  end
endmodule
