// @measure: run
// @work: array pass
//
// One pass writes every element into a single wide bitvector by indexed
// part-select. Inserting into a wide value is where this differs from writing
// a two-dimensional packed array, which sits beside it.
module Top;
  localparam int ARRAY_SIZE = 1024;

  logic [32*ARRAY_SIZE-1:0] data;
  int sum;

  initial begin
    int num_passes;

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 10;

    data = '0;
    for (int pass = 0; pass < num_passes; pass++) begin
      for (int i = 0; i < ARRAY_SIZE; i++)
        data[i*32 +: 32] = 32'(i * 3 + pass);
    end

    sum = 0;
    for (int i = 0; i < ARRAY_SIZE; i++)
      sum = sum + int'(data[i*32 +: 32]);

    $display("packed-slice-write done: sum=%0d", sum);
    $finish;
  end
endmodule
