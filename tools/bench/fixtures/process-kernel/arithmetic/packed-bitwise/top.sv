`ifndef NUM_ITERS
`define NUM_ITERS 10000000
`endif

// Wide packed bitvector operations: XOR, concatenation-swap, part-select,
// byte extraction. Tests the 4-state wide-integer code path for bitwise
// and slice operations. No arithmetic (add/mul) on wide types -- that would
// conflate width with operation type.

module Top;
  localparam int NUM_ITERS = `NUM_ITERS;

  initial begin
    logic [255:0] wide;
    logic [7:0] sum;

    wide = 256'hA5A5_A5A5_DEAD_BEEF_CAFE_BABE_1234_5678_FEDC_BA98_7654_3210_0BAD_F00D_FACE_B00C;
    sum = 0;

    for (int i = 0; i < NUM_ITERS; i++) begin
      wide = wide ^ {wide[127:0], wide[255:128]};
      wide[31:0] = wide[63:32] ^ wide[95:64];
      wide[255:224] = wide[223:192] ^ wide[191:160];
      sum = sum ^ wide[7:0];
    end

    $display("packed-bitwise done: sum=%0d", sum);
    $finish;
  end
endmodule
