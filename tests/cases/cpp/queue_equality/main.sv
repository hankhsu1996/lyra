// LRM 11.2.2 + 11.4.5 logical and case equality on queues. Aggregate == reduces
// element-wise comparisons through &&; runtime size mismatch yields 0 and
// empty-vs-empty yields 1 (industry convention, LRM 11.2.2 silent). == / !=
// propagate X / Z (LRM 11.4.5); === / !== match X / Z as values.
module Top;
  int a [$] = '{10, 20, 30};
  int b [$] = '{10, 20, 30};
  int shorter [$] = '{10, 20};
  int diff_val [$] = '{10, 20, 99};
  int empty1 [$];
  int empty2 [$];

  logic [3:0] u_with_x [$] = '{4'b1010, 4'b10x0, 4'b1111};
  logic [3:0] u_no_x   [$] = '{4'b1010, 4'b1010, 4'b1111};
  logic [3:0] u_x_same [$] = '{4'b1010, 4'b10x0, 4'b1111};

  bit eq_1d, ne_1d;
  bit eq_size_mismatch, ne_size_mismatch;
  bit eq_diff_val, ne_diff_val;
  bit eq_empty_empty, ne_empty_empty;
  bit eq_empty_nonempty;
  logic eq_with_x, ne_with_x;
  bit ce_1d, nce_1d;
  bit ce_size_mismatch;
  bit ce_x_match, ce_x_mismatch;

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
    eq_with_x = (u_with_x == u_no_x);
    ne_with_x = (u_with_x != u_no_x);
    ce_1d = (a === b);
    nce_1d = (a !== b);
    ce_size_mismatch = (a === shorter);
    ce_x_match = (u_with_x === u_x_same);
    ce_x_mismatch = (u_with_x === u_no_x);
  end
endmodule
