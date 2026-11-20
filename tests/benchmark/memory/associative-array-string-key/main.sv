// @measure: run
// @work: array pass
//
// The work of associative-array-int-key with a string index: one pass writes
// 1024 entries, asks whether each exists and reads it, walks every entry in
// index order with first and next, and deletes each. A string index is ordered
// as strings compare (LRM 7.8.2), so what a pass costs is the same operations
// with an index whose comparison reads its characters.
module Top;
  localparam int ENTRIES = 1024;
  localparam int STRIDE = 7919;

  initial begin
    int num_passes;
    int checksum;
    string keys[ENTRIES];
    int a[string];
    string index;

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 1;

    for (int i = 0; i < ENTRIES; i++)
      keys[i] = $sformatf("key %0d", (i * STRIDE) % (ENTRIES * 8));

    checksum = 0;
    for (int pass = 0; pass < num_passes; pass++) begin
      for (int i = 0; i < ENTRIES; i++) a[keys[i]] = i + pass;
      for (int i = 0; i < ENTRIES; i++)
        if (a.exists(keys[i])) checksum = checksum + a[keys[i]];
      if (a.first(index))
        do checksum = checksum + index.len(); while (a.next(index));
      for (int i = 0; i < ENTRIES; i++) a.delete(keys[i]);
    end

    $display("associative-array-string-key done: checksum=%0d", checksum);
    $finish;
  end
endmodule
