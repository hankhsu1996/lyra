module Top;
  typedef logic [3:0] mubi_t;
  typedef struct packed {
    logic a;
    logic [2:0] b;
  } sp_t;
  typedef enum logic [1:0] {E0, E1, E2} e_t;

  mubi_t q;
  bit [1:0][7:0] m2d;

  int bits_type_rt;
  int bits_value_rt;
  int bits_struct_rt;
  int bits_enum_rt;
  int bits_2d_rt;

  // `$bits` used as a part-select bound: the `$bits`-derived high bound is
  // `integer` (4-state), the literal low bound is `int` (2-state), so the
  // endpoint-ordering comparison must reconcile the two domains.
  logic [2:0] slice_rt;
  logic red_rt;
  logic ca_red;
  assign ca_red = ^q[$bits(mubi_t)-1:1];

  initial begin
    q = 4'b1010;
    bits_type_rt = $bits(mubi_t);
    bits_value_rt = $bits(q);
    bits_struct_rt = $bits(sp_t);
    bits_enum_rt = $bits(e_t);
    bits_2d_rt = $bits(m2d);
    slice_rt = q[$bits(mubi_t)-1:1];
    red_rt = ^q[$bits(mubi_t)-1:1];
  end
endmodule
