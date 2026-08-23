module Top;
  // LRM 20.9 over a 4-state packed operand: one bit of each four-state value
  // sits beside a run of known bits, so every control bit selects a distinct
  // non-empty subset.
  logic [7:0] mixed = 8'b1010_01xz;
  int r_ones;
  int r_zeros;
  int r_xs;
  int r_zs;
  int r_repeated_control;
  int r_every_control;
  int r_unknown_control;

  // LRM 20.9 over a 2-state operand: it has no x or z bits to match, and its
  // zero count must stop at the declared width rather than run to the end of
  // the storage word.
  bit [7:0] known = 8'b1010_0100;
  int r_two_state_xs;
  int r_two_state_ones;
  int r_two_state_zeros;

  // A count that crosses word boundaries, with a set bit on each side of one
  // and at both ends of the value.
  logic [99:0] wide;
  int r_wide_ones;
  int r_wide_zeros;

  // LRM 20.9 takes any bit-stream operand (LRM 6.24.3), so an unpacked struct,
  // an unpacked array, and a string each count over the bits they hold.
  typedef struct {
    int p;
    logic [3:0] q;
  } sp_t;
  sp_t sp;
  int r_struct_ones;
  logic [3:0] arr[2];
  int r_array_ones;
  string s = "A";
  int r_string_ones;
  int r_string_zeros;

  // LRM 20.9 one-hot readings. An x bit is not a one, so a lone x leaves a
  // single set bit still one-hot.
  bit r_onehot;
  bit r_onehot_none;
  bit r_onehot_many;
  bit r_onehot_with_x;
  bit r_onehot0_none;
  bit r_onehot0_many;

  // A simulation-time value expression driving a net.
  logic [7:0] bus = 8'b1101_0000;
  int ca_ones;
  bit ca_onehot;
  assign ca_ones = $countones(bus);
  assign ca_onehot = $onehot(bus);

  initial begin
    r_ones = $countbits(mixed, 1'b1);
    r_zeros = $countbits(mixed, 1'b0);
    r_xs = $countbits(mixed, 1'bx);
    r_zs = $countbits(mixed, 1'bz);
    r_repeated_control = $countbits(mixed, 1'b1, 1'b1);
    r_every_control = $countbits(mixed, 1'b0, 1'b1, 1'bx, 1'bz);
    r_unknown_control = $countbits(mixed, 1'bx, 1'bz);

    r_two_state_xs = $countbits(known, 1'bx);
    r_two_state_ones = $countones(known);
    r_two_state_zeros = $countbits(known, 1'b0);

    wide = 100'b0;
    wide[99] = 1'b1;
    wide[64] = 1'b1;
    wide[63] = 1'b1;
    wide[0] = 1'b1;
    r_wide_ones = $countones(wide);
    r_wide_zeros = $countbits(wide, 1'b0);

    sp.p = 3;
    sp.q = 4'b1001;
    r_struct_ones = $countones(sp);
    arr[0] = 4'b1100;
    arr[1] = 4'b0001;
    r_array_ones = $countones(arr);
    r_string_ones = $countones(s);
    r_string_zeros = $countbits(s, 1'b0);

    r_onehot = $onehot(8'b0010_0000);
    r_onehot_none = $onehot(8'b0000_0000);
    r_onehot_many = $onehot(8'b1010_0000);
    r_onehot_with_x = $onehot(8'b0000_001x);
    r_onehot0_none = $onehot0(8'b0000_0000);
    r_onehot0_many = $onehot0(8'b1010_0000);
  end
endmodule
