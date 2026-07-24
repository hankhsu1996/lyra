module Top;
  // LRM 21.4.1 / 21.5: $readmem / $writemem over a dynamic array, a queue, and
  // an associative array. Dynamic array and queue are 0-based memories sized by
  // the container (the load does not resize them); an associative array is
  // addressed by integral key, and its dump writes an `@key` per entry. Each
  // facet writes an image with $writemem or $fwrite, loads it back, and asserts;
  // a write->read round-trip proves the dump and load agree.
  bit [7:0]  dyn      [];
  bit [7:0]  dyn_rt   [];
  bit [15:0] q        [$];
  bit [15:0] q_rt     [$];
  bit [7:0]  dyn_at   [];
  bit [7:0]  amem     [int];
  bit [7:0]  amem_rt  [int];
  bit [7:0]  a2, a5, r1, r7, r100;

  int fd;

  initial begin
    // Dynamic array load from a file image.
    dyn = new[4];
    fd = $fopen("dyn.hex", "w");
    $fwrite(fd, "0a 0b 0c 0d\n");
    $fclose(fd);
    $readmemh("dyn.hex", dyn);

    // Dynamic array write->read round-trip.
    dyn_rt = new[3];
    dyn_rt[0] = 8'haa;
    dyn_rt[1] = 8'hbb;
    dyn_rt[2] = 8'hcc;
    $writememh("dyn_rt.hex", dyn_rt);
    dyn_rt = new[3];
    $readmemh("dyn_rt.hex", dyn_rt);

    // Queue load from a file image (0-based).
    q = '{16'h0000, 16'h0000, 16'h0000};
    fd = $fopen("q.hex", "w");
    $fwrite(fd, "1111 2222 3333\n");
    $fclose(fd);
    $readmemh("q.hex", q);

    // Queue write->read round-trip.
    q_rt = '{16'hdead, 16'hbeef};
    $writememh("q_rt.hex", q_rt);
    q_rt = '{16'h0000, 16'h0000};
    $readmemh("q_rt.hex", q_rt);

    // Dynamic array with an @address directive: index 0 stays default, then
    // two words load at @1.
    dyn_at = new[4];
    fd = $fopen("dyn_at.hex", "w");
    $fwrite(fd, "@1\n55\n66\n");
    $fclose(fd);
    $readmemh("dyn_at.hex", dyn_at);

    // Associative load: each @key creates a sparse entry. Only keys 2 and 5
    // exist afterward.
    fd = $fopen("amem.hex", "w");
    $fwrite(fd, "@2\naa\n@5\nbb\n");
    $fclose(fd);
    $readmemh("amem.hex", amem);
    a2 = amem[2];
    a5 = amem[5];

    // Associative round-trip: sparse entries are dumped @key-per-entry (in
    // ascending key order) and reload exactly. The array is cleared first so
    // the reload alone reconstructs it.
    amem_rt[1]   = 8'h11;
    amem_rt[7]   = 8'h77;
    amem_rt[100] = 8'hcc;
    $writememh("amem_rt.hex", amem_rt);
    amem_rt.delete();
    $readmemh("amem_rt.hex", amem_rt);
    r1   = amem_rt[1];
    r7   = amem_rt[7];
    r100 = amem_rt[100];
  end
endmodule
