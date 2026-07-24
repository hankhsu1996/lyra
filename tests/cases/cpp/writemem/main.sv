module Top;
  // LRM 21.5 $writememh / $writememb. Each memory is written to a file and read
  // back with the matching $readmem, so a value that round-trips proves both the
  // dump format and its addressing. Facets: full hex round-trip, full binary
  // round-trip, a start/finish sub-range (no @address is written for an unpacked
  // array, LRM 21.5.3), and a 4-state binary round-trip (bit-exact x/z).
  bit [31:0]   src   [0:3];
  bit [31:0]   dst   [0:3];
  bit [7:0]    bsrc  [0:2];
  bit [7:0]    bdst  [0:2];
  bit [31:0]   rsrc  [0:3];
  bit [31:0]   rdst  [0:1];
  logic [3:0]  xsrc  [0:2];
  logic [3:0]  xdst  [0:2];

  initial begin
    // Full hex round-trip.
    src = '{32'hDEADBEEF, 32'h0a0b0c0d, 32'h11223344, 32'hcafef00d};
    $writememh("out.hex", src);
    $readmemh("out.hex", dst);

    // Full binary round-trip.
    bsrc = '{8'ha5, 8'h3c, 8'hff};
    $writememb("out.bin", bsrc);
    $readmemb("out.bin", bdst);

    // start/finish sub-range: only rsrc[1] and rsrc[2] are written, as two
    // words with no @address, then loaded into rdst[0] and rdst[1].
    rsrc = '{32'h11111111, 32'h22222222, 32'h33333333, 32'h44444444};
    $writememh("range.hex", rsrc, 1, 2);
    $readmemh("range.hex", rdst);

    // 4-state binary round-trip: x/z survive bit-exact through $writememb.
    xsrc = '{4'b10x1, 4'bz0z0, 4'b1x0z};
    $writememb("x.bin", xsrc);
    $readmemb("x.bin", xdst);
  end
endmodule
