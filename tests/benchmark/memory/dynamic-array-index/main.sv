// @measure: run
// @work: array pass
//
// One pass reads every element of a dynamic array of 4096 through its index
// and writes each back changed, so what a pass costs is reaching an element of
// a dynamic array where it lies, to read it and to write it (LRM 7.5).
module Top;
  localparam int ARRAY_SIZE = 4096;

  initial begin
    int num_passes;
    int checksum;
    int a[];
    int element;

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 1;

    a = new[ARRAY_SIZE];
    for (int i = 0; i < ARRAY_SIZE; i++) a[i] = i;

    checksum = 0;
    for (int pass = 0; pass < num_passes; pass++) begin
      for (int i = 0; i < ARRAY_SIZE; i++) begin
        element = a[i];
        checksum = checksum + element;
        a[i] = element + pass;
      end
    end

    $display("dynamic-array-index done: checksum=%0d", checksum);
    $finish;
  end
endmodule
