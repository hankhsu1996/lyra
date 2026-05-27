module Top;
  int neg_value_match;
  int neg_range_match;
  int neg_range_at_lo;
  int neg_range_at_hi;
  int pos_outside_neg_range;
  initial begin
    int v;
    v = -5;  neg_value_match = (v inside {-1, -5, -9}) ? 1 : 0;
    v = -5;  neg_range_match = (v inside {[-10:-1]}) ? 1 : 0;
    v = -10; neg_range_at_lo = (v inside {[-10:-1]}) ? 1 : 0;
    v = -1;  neg_range_at_hi = (v inside {[-10:-1]}) ? 1 : 0;
    v = 1;   pos_outside_neg_range = (v inside {[-10:-1]}) ? 1 : 0;
  end
endmodule
