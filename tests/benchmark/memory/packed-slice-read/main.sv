// @measure: run
// @work: array pass
//
// One pass reads every element out of a single wide bitvector by indexed
// part-select, which is the other way a designer spells an array of packed
// values. Beside the two-dimensional form, the pair says what the spelling
// costs rather than what the storage does.
module Top;
  localparam int ARRAY_SIZE = 1024;

  logic [32*ARRAY_SIZE-1:0] data;
  int sum;

  initial begin
    int num_passes;

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 10;

    for (int i = 0; i < ARRAY_SIZE; i++)
      data[i*32 +: 32] = 32'(i * 3 + 1);

    sum = 0;
    for (int pass = 0; pass < num_passes; pass++) begin
      for (int i = 0; i < ARRAY_SIZE; i++)
        sum = sum + int'(data[i*32 +: 32]);
    end

    $display("packed-slice-read done: sum=%0d", sum);
    $finish;
  end
endmodule
