// @measure: run
// @work: array pass
//
// One pass writes 1024 entries of an associative array with an integral index,
// asks whether each exists and reads it, walks every entry in index order with
// first and next, and deletes each, so what a pass costs is an associative
// array's own operations: making an entry, finding one by its index, stepping
// from one to the next, and removing one (LRM 7.8, 7.9).
module Top;
  localparam int ENTRIES = 1024;
  // Spreads the indices so their order is not the order they are written in.
  localparam int STRIDE = 7919;

  initial begin
    int num_passes;
    int checksum;
    int a[int];
    int index;

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 1;

    checksum = 0;
    for (int pass = 0; pass < num_passes; pass++) begin
      for (int i = 0; i < ENTRIES; i++)
        a[(i * STRIDE) % (ENTRIES * 8)] = i + pass;
      for (int i = 0; i < ENTRIES; i++)
        if (a.exists((i * STRIDE) % (ENTRIES * 8)))
          checksum = checksum + a[(i * STRIDE) % (ENTRIES * 8)];
      if (a.first(index))
        do checksum = checksum + index; while (a.next(index));
      for (int i = 0; i < ENTRIES; i++) a.delete((i * STRIDE) % (ENTRIES * 8));
    end

    $display("associative-array-int-key done: checksum=%0d", checksum);
    $finish;
  end
endmodule
