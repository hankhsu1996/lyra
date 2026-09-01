// @measure: run
// @work: iteration
//
// Bitwise work on a 256-bit packed value: XOR, a concatenation that rotates
// it, part-selects, and a byte extraction. No arithmetic, so what an iteration
// costs is the wide four-state bitwise and slice paths rather than a carry
// chain.
module Top;
  initial begin
    int num_iters;
    logic [255:0] wide;
    logic [7:0] sum;

    if (!$value$plusargs("work=%d", num_iters)) num_iters = 1000;

    wide = 256'hA5A5_A5A5_DEAD_BEEF_CAFE_BABE_1234_5678_FEDC_BA98_7654_3210_0BAD_F00D_FACE_B00C;
    sum = 0;

    for (int i = 0; i < num_iters; i++) begin
      wide = wide ^ {wide[127:0], wide[255:128]};
      wide[31:0] = wide[63:32] ^ wide[95:64];
      wide[255:224] = wide[223:192] ^ wide[191:160];
      sum = sum ^ wide[7:0];
    end

    $display("packed-bitwise done: sum=%0d", sum);
    $finish;
  end
endmodule
