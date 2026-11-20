// @measure: run
// @work: array pass
//
// One pass makes a dynamic array of 256 elements, writes one of them, then
// makes it 1024 long keeping those 256 (LRM 7.5.1), so what a pass costs is
// taking a block of elements, filling it with the default and with what the
// array held, and letting go of the block it replaces.
module Top;
  localparam int ARRAY_SIZE = 1024;
  localparam int KEPT = ARRAY_SIZE / 4;

  initial begin
    int num_passes;
    int checksum;
    int a[];

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 1;

    checksum = 0;
    for (int pass = 0; pass < num_passes; pass++) begin
      a = new[KEPT];
      a[pass % KEPT] = pass;
      a = new[ARRAY_SIZE](a);
      checksum = checksum + a[pass % KEPT] + a.size();
    end

    $display("dynamic-array-resize done: checksum=%0d", checksum);
    $finish;
  end
endmodule
