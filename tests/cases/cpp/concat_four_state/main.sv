module Top;
  logic [7:0] x_high;
  logic [7:0] known_low;
  logic [15:0] x_propagated;
  logic [7:0] z_known;
  logic [3:0] xz_mixed;
  int x_compare_known;
  int z_compare_known;
  int xz_self_eq;

  initial begin
    x_high = 8'b1010xxxx;
    known_low = 8'hCD;
    x_propagated = {x_high, known_low};
    z_known = 8'bzzzz1100;
    xz_mixed = 4'bx10z;
    x_compare_known = (x_propagated === 16'hABCD) ? 1 : 0;
    z_compare_known = (z_known === 8'hFC) ? 1 : 0;
    xz_self_eq = (xz_mixed === xz_mixed) ? 1 : 0;
  end
endmodule
