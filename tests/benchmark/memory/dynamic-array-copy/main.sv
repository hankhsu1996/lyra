// @measure: run
// @work: array pass
//
// One pass changes one element of a dynamic array of 4096 elements, writes a
// copy of the whole array over another, and compares the two, so what a pass
// costs is a dynamic array's operations on a whole value: copying it over one
// that holds elements of its own, ending those, and its equality.
module Top;
  localparam int ARRAY_SIZE = 4096;

  initial begin
    int num_passes;
    int checksum;
    int source[];
    int copy[];

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 1;

    source = new[ARRAY_SIZE];
    copy = new[ARRAY_SIZE];
    for (int i = 0; i < ARRAY_SIZE; i++) source[i] = i;

    checksum = 0;
    for (int pass = 0; pass < num_passes; pass++) begin
      source[pass % ARRAY_SIZE] = pass;
      copy = source;
      if (copy == source) checksum = checksum + copy[pass % ARRAY_SIZE];
    end

    $display("dynamic-array-copy done: checksum=%0d", checksum);
    $finish;
  end
endmodule
