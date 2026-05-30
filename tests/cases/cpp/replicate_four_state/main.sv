module Top;
  logic [3:0] xz_pattern;
  logic [11:0] propagated;
  int eq_known;
  int eq_self;
  int neq_self;

  initial begin
    xz_pattern = 4'bx01z;
    propagated = {3{xz_pattern}};
    eq_known = (propagated === 12'h000) ? 1 : 0;
    eq_self = (propagated === propagated) ? 1 : 0;
    neq_self = (propagated !== propagated) ? 1 : 0;
  end
endmodule
