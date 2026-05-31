module Top;
  bit [127:0] wide_zero_ext;
  bit signed [127:0] wide_sign_ext;
  int narrow_trunc;
  int narrow_signed;
  bit [127:0] longint_to_wide;
  longint longint_roundtrip;
  initial begin
    int n_unsigned;
    int n_signed;
    bit [127:0] w_unsigned;
    bit signed [127:0] w_signed;
    longint l_in;

    n_unsigned = 32'h12345678;
    wide_zero_ext = n_unsigned;

    n_signed = -1;
    wide_sign_ext = n_signed;

    w_unsigned = 128'hDEADBEEF_12345678;
    narrow_trunc = w_unsigned;

    w_signed = -1;
    narrow_signed = w_signed;

    l_in = 64'h123456789ABCDEF0;
    longint_to_wide = l_in;
    longint_roundtrip = longint_to_wide;
  end
endmodule
