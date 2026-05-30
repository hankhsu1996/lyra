module Top;
  logic [3:0] all_x;
  logic [3:0] all_z;
  logic [3:0] mixed;
  int x_eq_x;
  int z_eq_z;
  int x_eq_z;
  int mixed_eq_self;
  int mixed_neq_self;
  int x_vs_known;
  int z_vs_known;

  initial begin
    all_x = 4'bxxxx;
    all_z = 4'bzzzz;
    mixed = 4'b10x1;

    x_eq_x = (all_x === all_x) ? 1 : 0;
    z_eq_z = (all_z === all_z) ? 1 : 0;
    x_eq_z = (all_x === all_z) ? 1 : 0;
    mixed_eq_self = (mixed === mixed) ? 1 : 0;
    mixed_neq_self = (mixed !== mixed) ? 1 : 0;
    x_vs_known = (mixed === 4'b1011) ? 1 : 0;
    z_vs_known = (4'b10z1 === 4'b1011) ? 1 : 0;
  end
endmodule
