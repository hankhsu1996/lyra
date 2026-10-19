// $writememb and $writememh dump a memory to a file readable by $readmemb and
// $readmemh, so what is written comes back bit for bit, x and z digits
// included. For an unpacked or dynamic array no address specifiers are
// written, so a dump of a sub-range reloads from the memory's lowest address;
// for an associative array they are written, so a sparse memory reloads at the
// indices it was dumped from (LRM 21.5, 21.5.1, 21.5.3). A slice is dumped by
// the indices it holds of the array it is taken from, and a range reaching past
// the memory at either end dumps the addresses the memory holds (LRM 7.4.5).
module Top;
  bit [31:0] source[0:3];
  bit [31:0] reloaded[0:3];

  logic [3:0] unknown_source[0:2];
  logic [3:0] unknown_reloaded[0:2];

  bit [31:0] windowed_source[0:3];
  bit [31:0] windowed_reloaded[0:1];

  bit [7:0] sparse[int];
  int sparse_count;

  bit [7:0] sliced_source[8];
  bit [7:0] sliced_reloaded[0:1];
  bit [7:0] sliced_expected[0:1] = '{8'h13, 8'h14};

  bit [7:0] overhung_source[0:3] = '{8'h31, 8'h32, 8'h33, 8'h34};
  bit [7:0] below_reloaded[0:3];
  bit [7:0] above_reloaded[0:3];
  bit [7:0] below_expected[0:3] = '{8'h31, 8'h32, 0, 0};
  bit [7:0] above_expected[0:3] = '{8'h33, 8'h34, 0, 0};
  bit [7:0] from_below_reloaded[0:3];
  bit [7:0] down_reloaded[0:3];
  bit [7:0] down_expected[0:3] = '{8'h34, 8'h33, 0, 0};

  initial begin
    source = '{32'hDEADBEEF, 32'h0a0b0c0d, 32'h11223344, 32'hcafef00d};
    $writememh("source.hex", source);
    $readmemh("source.hex", reloaded);

    unknown_source = '{4'b10x1, 4'bz0z0, 4'b1x0z};
    $writememb("unknown.bin", unknown_source);
    $readmemb("unknown.bin", unknown_reloaded);

    windowed_source = '{32'h11111111, 32'h22222222, 32'h33333333,
                        32'h44444444};
    $writememh("windowed.hex", windowed_source, 1, 2);
    $readmemh("windowed.hex", windowed_reloaded);

    sparse[1] = 8'h11;
    sparse[7] = 8'h77;
    sparse[100] = 8'hcc;
    $writememh("sparse.hex", sparse);
    sparse.delete();
    $readmemh("sparse.hex", sparse);
    sparse_count = sparse.num();

    for (int i = 0; i < 8; i++) sliced_source[i] = 8'(i + 8'h10);
    $writememh("sliced.hex", sliced_source[2:5], 3, 4);
    $readmemh("sliced.hex", sliced_reloaded);

    $writememh("below.hex", overhung_source, -2, 1);
    $readmemh("below.hex", below_reloaded);
    $writememh("above.hex", overhung_source, 2, 9);
    $readmemh("above.hex", above_reloaded);
    $writememh("from_below.hex", overhung_source, -2);
    $readmemh("from_below.hex", from_below_reloaded);
    $writememh("down.hex", overhung_source, 5, 2);
    $readmemh("down.hex", down_reloaded);
  end

  final begin
    if (reloaded[0] !== 32'hDEADBEEF)
      $fatal(1, "reloaded[0] was %h, expected deadbeef", reloaded[0]);
    if (reloaded[1] !== 32'h0a0b0c0d)
      $fatal(1, "reloaded[1] was %h, expected 0a0b0c0d", reloaded[1]);
    if (reloaded[2] !== 32'h11223344)
      $fatal(1, "reloaded[2] was %h, expected 11223344", reloaded[2]);
    if (reloaded[3] !== 32'hcafef00d)
      $fatal(1, "reloaded[3] was %h, expected cafef00d", reloaded[3]);

    if (unknown_reloaded[0] !== 4'b10x1)
      $fatal(1, "unknown_reloaded[0] was %b, expected 10x1",
             unknown_reloaded[0]);
    if (unknown_reloaded[1] !== 4'bz0z0)
      $fatal(1, "unknown_reloaded[1] was %b, expected z0z0",
             unknown_reloaded[1]);
    if (unknown_reloaded[2] !== 4'b1x0z)
      $fatal(1, "unknown_reloaded[2] was %b, expected 1x0z",
             unknown_reloaded[2]);

    if (windowed_reloaded[0] !== 32'h22222222)
      $fatal(1, "windowed_reloaded[0] was %h, expected 22222222",
             windowed_reloaded[0]);
    if (windowed_reloaded[1] !== 32'h33333333)
      $fatal(1, "windowed_reloaded[1] was %h, expected 33333333",
             windowed_reloaded[1]);

    if (sparse_count !== 3)
      $fatal(1, "the reloaded associative array held %0d entries, expected 3",
             sparse_count);
    if (sparse[1] !== 8'h11)
      $fatal(1, "sparse[1] was %h, expected 11", sparse[1]);
    if (sparse[7] !== 8'h77)
      $fatal(1, "sparse[7] was %h, expected 77", sparse[7]);
    if (sparse[100] !== 8'hcc)
      $fatal(1, "sparse[100] was %h, expected cc", sparse[100]);

    if (sliced_reloaded !== sliced_expected)
      $fatal(1, "a slice dumped from index 3 to 4 reloaded as %p, expected 13 and 14",
             sliced_reloaded);
    if (below_reloaded !== below_expected)
      $fatal(1, "a dump from below the memory reloaded as %p, expected 31 and 32",
             below_reloaded);
    if (above_reloaded !== above_expected)
      $fatal(1, "a dump past the memory reloaded as %p, expected 33 and 34",
             above_reloaded);
    if (from_below_reloaded !== overhung_source)
      $fatal(1, "a dump from a start below the memory reloaded as %p, expected %p",
             from_below_reloaded, overhung_source);
    if (down_reloaded !== down_expected)
      $fatal(1, "a dump counting down into the memory reloaded as %p, expected 34 and 33",
             down_reloaded);
    $display("All checks passed");
  end
endmodule
