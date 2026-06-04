// LRM 11.4.5 case equality on dynamic arrays. `===` / `!==` always yield 0
// or 1; X and Z are matched as values (not propagated). Size mismatch follows
// the same convention as `==` (industry: runtime size mismatch yields 0).
// Multi-dim and mixed-container nesting compose via the recursive element
// type.
module Top;
  int a [] = '{10, 20, 30};
  int b [] = '{10, 20, 30};
  int shorter [] = '{10, 20};
  int diff_val [] = '{10, 20, 99};
  int empty1 [];
  int empty2 [];

  int m [][] = '{'{1, 2, 3}, '{4, 5, 6}};
  int n [][] = '{'{1, 2, 3}, '{4, 5, 6}};

  int fdo_p [2][] = '{'{1, 2}, '{3, 4, 5}};
  int fdo_q [2][] = '{'{1, 2}, '{3, 4, 5}};

  int dfo_x [][3] = '{'{1, 2, 3}, '{4, 5, 6}};
  int dfo_y [][3] = '{'{1, 2, 3}, '{4, 5, 6}};

  // 4-state: X matched as value -> 1 when same X positions, 0 when X vs 0.
  logic [3:0] u_x_a [] = '{4'b1010, 4'b10x0, 4'b1111};
  logic [3:0] u_x_b [] = '{4'b1010, 4'b10x0, 4'b1111};
  logic [3:0] u_x_c [] = '{4'b1010, 4'b1010, 4'b1111};

  bit ce_1d, nce_1d;
  bit ce_size_mismatch, nce_size_mismatch;
  bit ce_diff_val, nce_diff_val;
  bit ce_empty_empty, nce_empty_empty;
  bit ce_empty_nonempty, nce_empty_nonempty;
  bit ce_2d, ce_fixed_outer, ce_dyn_outer;
  bit ce_x_match, ce_x_mismatch;

  initial begin
    ce_1d = (a === b);
    nce_1d = (a !== b);
    ce_size_mismatch = (a === shorter);
    nce_size_mismatch = (a !== shorter);
    ce_diff_val = (a === diff_val);
    nce_diff_val = (a !== diff_val);

    ce_empty_empty = (empty1 === empty2);
    nce_empty_empty = (empty1 !== empty2);
    ce_empty_nonempty = (empty1 === a);
    nce_empty_nonempty = (empty1 !== a);

    ce_2d = (m === n);
    ce_fixed_outer = (fdo_p === fdo_q);
    ce_dyn_outer = (dfo_x === dfo_y);

    ce_x_match = (u_x_a === u_x_b);
    ce_x_mismatch = (u_x_a === u_x_c);
  end
endmodule
