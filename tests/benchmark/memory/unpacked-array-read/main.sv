// @measure: run
// @work: array pass
//
// One pass reads every element of a 32768-element unpacked array and sums it,
// so what a pass costs is the load path and the index computation behind it.
// The array is filled before any of that, which is setup and not the subject.
module Top;
  localparam int ARRAY_SIZE = 32768;

  logic [31:0] data [0:ARRAY_SIZE-1];

  initial begin
    int num_passes;
    int sum;

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 1;

    for (int i = 0; i < ARRAY_SIZE; i++) data[i] = i * 3 + 1;

    sum = 0;
    for (int pass = 0; pass < num_passes; pass++) begin
      for (int i = 0; i < ARRAY_SIZE; i++) sum = sum + int'(data[i]);
    end

    $display("unpacked-array-read done: sum=%0d", sum);
    $finish;
  end
endmodule
