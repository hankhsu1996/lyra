// @measure: run
// @work: iteration
//
// Arithmetic on packed values wider than a machine's widest integer: a
// 256-bit four-state multiply-accumulate with a carry rippling through every
// word, a comparison, a variable shift and a remainder, and a 1024-bit add and
// equality, so what an iteration costs is the carry, product and quotient
// paths over several words rather than one word's instructions.
module Top;
  initial begin
    int num_iters;
    logic [255:0] acc, x, m;
    logic [1023:0] big, step;
    logic [7:0] sum;

    if (!$value$plusargs("work=%d", num_iters)) num_iters = 1000;

    acc = 256'h0123_4567_89AB_CDEF_FEDC_BA98_7654_3210_0F1E_2D3C_4B5A_6978_8796_A5B4_C3D2_E1F0;
    x = 256'h9E37_79B9_7F4A_7C15_F39C_C060_5CED_C834_1082_276B_F3A2_7251_F86C_6A11_D0C1_8E95;
    m = {1'b1, 255'd0} - 256'd189;
    big = {4{acc}};
    step = {4{x}};
    sum = 0;

    for (int i = 0; i < num_iters; i++) begin
      acc = acc * x + {248'd0, sum};
      if (acc > m) acc = acc - m;
      x = x ^ (acc >> (i % 61));
      big = big + step;
      if (big == step) sum = sum + 1;
      if (i % 16 == 0) acc = acc % (m >> 128);
      sum = sum ^ acc[7:0] ^ big[1023:1016];
    end

    $display("wide-arith done: sum=%0d", sum);
    $finish;
  end
endmodule
