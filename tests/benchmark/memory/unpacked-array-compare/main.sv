// @measure: run
// @work: array pass
//
// One pass changes one element of a fixed-size unpacked array of 4096
// elements and compares it with another whole, so what a pass costs is the
// array's equality on a whole value, element by element (LRM 11.4.5).
module Top;
  localparam int ARRAY_SIZE = 4096;

  logic [31:0] left [0:ARRAY_SIZE-1];
  logic [31:0] right [0:ARRAY_SIZE-1];

  initial begin
    int num_passes;
    int checksum;

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 1;

    for (int i = 0; i < ARRAY_SIZE; i++) begin
      left[i] = i;
      right[i] = i;
    end

    checksum = 0;
    for (int pass = 0; pass < num_passes; pass++) begin
      left[pass % ARRAY_SIZE] = pass % ARRAY_SIZE;
      if (left == right) checksum = checksum + 1;
    end

    $display("unpacked-array-compare done: checksum=%0d", checksum);
    $finish;
  end
endmodule
