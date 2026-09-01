// @measure: run
// @work: array pass
//
// One pass reads every element of a packed array declared with two dimensions
// and indexed element by element, which is the source-level form a designer
// writes. The unpacked counterpart sits beside it, so the pair says what the
// choice of storage costs.
module Top;
  localparam int ARRAY_SIZE = 1024;

  logic [ARRAY_SIZE-1:0][31:0] data;
  int sum;

  initial begin
    int num_passes;

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 10;

    for (int i = 0; i < ARRAY_SIZE; i++) data[i] = 32'(i * 3 + 1);

    sum = 0;
    for (int pass = 0; pass < num_passes; pass++) begin
      for (int i = 0; i < ARRAY_SIZE; i++) sum = sum + int'(data[i]);
    end

    $display("packed-array-read done: sum=%0d", sum);
    $finish;
  end
endmodule
