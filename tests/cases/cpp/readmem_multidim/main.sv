module Top;
  // LRM 21.4.3: $readmem / $writemem over a multidimensional memory. The file is
  // row-major with the lowest dimension varying fastest; an `@address` targets
  // the highest dimension's words; the file is ascending-address ordered even
  // when a dimension is declared descending. Each facet probes leaves into
  // scalars so the assertions read them directly.
  bit [7:0] full [0:1][0:2];
  bit [7:0] at   [0:2][0:1];
  bit [7:0] desc [2:0][0:1];
  bit [7:0] m3   [0:1][0:1][0:1];
  bit [7:0] src  [0:1][0:1];
  bit [7:0] dst  [0:1][0:1];

  bit [7:0] f00, f01, f02, f10, f11, f12;
  bit [7:0] a00, a10, a11, a20;
  bit [7:0] d00, d10, d20;
  bit [7:0] t000, t011, t100, t111;
  bit [7:0] r00, r01, r10, r11;

  int fd;

  initial begin
    // 2-D full row-major load.
    fd = $fopen("full.hex", "w");
    $fwrite(fd, "00 01 02 10 11 12\n");
    $fclose(fd);
    $readmemh("full.hex", full);
    f00 = full[0][0]; f01 = full[0][1]; f02 = full[0][2];
    f10 = full[1][0]; f11 = full[1][1]; f12 = full[1][2];

    // @address targets the highest dimension: only top-slot 1 loads, the rest
    // keep their default.
    fd = $fopen("at.hex", "w");
    $fwrite(fd, "@1\naa\nbb\n");
    $fclose(fd);
    $readmemh("at.hex", at);
    a00 = at[0][0]; a10 = at[1][0]; a11 = at[1][1]; a20 = at[2][0];

    // Descending highest dim [2:0]: file address 0 still loads index 0.
    fd = $fopen("desc.hex", "w");
    $fwrite(fd, "00 01 10 11 20 21\n");
    $fclose(fd);
    $readmemh("desc.hex", desc);
    d00 = desc[0][0]; d10 = desc[1][0]; d20 = desc[2][0];

    // Three dimensions, row-major (lowest varies fastest).
    fd = $fopen("m3.hex", "w");
    $fwrite(fd, "00 01 02 03 04 05 06 07\n");
    $fclose(fd);
    $readmemh("m3.hex", m3);
    t000 = m3[0][0][0]; t011 = m3[0][1][1]; t100 = m3[1][0][0]; t111 = m3[1][1][1];

    // writemem -> readmem round-trip.
    src[0][0] = 8'h1a; src[0][1] = 8'h2b;
    src[1][0] = 8'h3c; src[1][1] = 8'h4d;
    $writememh("rt.hex", src);
    $readmemh("rt.hex", dst);
    r00 = dst[0][0]; r01 = dst[0][1]; r10 = dst[1][0]; r11 = dst[1][1];
  end
endmodule
