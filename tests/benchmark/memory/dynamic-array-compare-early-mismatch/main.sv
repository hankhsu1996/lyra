// @measure: run
// @work: array pass
//
// One pass compares two dynamic arrays of 4096 elements that differ in their
// first element, so what a pass costs is an equality whose answer the first
// pair already settles (LRM 11.4.5): the opposite end from a comparison of
// two equal arrays, which has to read every pair.
module Top;
  localparam int ARRAY_SIZE = 4096;

  initial begin
    int num_passes;
    int checksum;
    int left[];
    int right[];

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 1;

    left = new[ARRAY_SIZE];
    right = new[ARRAY_SIZE];
    for (int i = 0; i < ARRAY_SIZE; i++) begin
      left[i] = i;
      right[i] = i;
    end

    checksum = 0;
    for (int pass = 0; pass < num_passes; pass++) begin
      left[0] = pass + 1;
      if (left != right) checksum = checksum + 1;
    end

    $display("dynamic-array-compare-early-mismatch done: checksum=%0d",
             checksum);
    $finish;
  end
endmodule
