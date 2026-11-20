// @measure: run
// @work: array pass
//
// The work of dynamic-array-copy over strings: one pass changes one element of
// a dynamic array of 4096 elements, writes a copy of the whole array over
// another, and compares the two. Each string is longer than a string keeps
// inside itself, so copying an element over another and comparing two reach
// storage of their own, which is what a dynamic array of an element type with
// a copy of its own costs.
module Top;
  localparam int ARRAY_SIZE = 4096;

  initial begin
    int num_passes;
    int checksum;
    string source[];
    string copy[];

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 1;

    source = new[ARRAY_SIZE];
    copy = new[ARRAY_SIZE];
    for (int i = 0; i < ARRAY_SIZE; i++)
      source[i] = $sformatf("element number %0d of the array", i);

    checksum = 0;
    for (int pass = 0; pass < num_passes; pass++) begin
      source[pass % ARRAY_SIZE] = source[(pass + 1) % ARRAY_SIZE];
      copy = source;
      if (copy == source)
        checksum = checksum + copy[pass % ARRAY_SIZE].len();
    end

    $display("dynamic-array-copy-string done: checksum=%0d", checksum);
    $finish;
  end
endmodule
