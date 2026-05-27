module Top;
  logic lhs_x_at_non_wildcard;
  logic lhs_z_at_non_wildcard;
  logic lhs_x_at_wildcard;
  logic lhs_all_x_rhs_all_wild;
  logic mismatch_dominates_x;
  logic ne_returns_x;
  initial begin
    logic [3:0] a;
    logic [3:0] b;

    a = 4'b101x; b = 4'b1010;
    lhs_x_at_non_wildcard = a ==? b;

    a = 4'b101z; b = 4'b1010;
    lhs_z_at_non_wildcard = a ==? b;

    a = 4'b101x; b = 4'b101z;
    lhs_x_at_wildcard = a ==? b;

    a = 4'bxxxx; b = 4'bzzzz;
    lhs_all_x_rhs_all_wild = a ==? b;

    a = 4'b001x; b = 4'b1010;
    mismatch_dominates_x = a ==? b;

    a = 4'b101x; b = 4'b1010;
    ne_returns_x = a !=? b;
  end
endmodule
