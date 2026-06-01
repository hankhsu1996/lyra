module Top;
  // Distinguishes 'b1 (numeric 1, cast per element) from '1 (unbased one,
  // fills the element type) -- LRM 10.9 / 5.7.1.
  logic [1:0][3:0] a;
  logic [1:0][3:0] b;
  int a_all;
  int b_all;
  initial begin
    a = '{default: 'b1};   // each 4-bit element = 4'b0001 -> packed 0x11
    b = '{default: '1};    // each 4-bit element = 4'b1111 -> packed 0xff
    a_all = a;
    b_all = b;
  end
endmodule
