// An associative array declared with a wildcard index type can be written
// whole by an assignment pattern, the same as one declared with an index type
// (LRM 7.9.11). Its indices are self-determined, so the pattern's keys need
// share no width with each other: what fixes the entry a key names is its
// numerical value alone, and a key written at one width is read back at
// another (LRM 7.8.1). A `default:` clause answers a read of an index the
// pattern gave no entry (LRM 7.8.6).
module Top;
  int wild [*] = '{8'd5: 100, 300: 7, default: -9};

  int narrow_written_wide_read;
  int wide_written;
  int absent;
  int count;

  initial begin
    narrow_written_wide_read = 0;
    wide_written = 0;
    absent = 0;
    count = 0;

    narrow_written_wide_read = wild[16'd5];
    wide_written = wild[300];
    absent = wild[1];
    count = wild.num();
  end

  final begin
    if (narrow_written_wide_read !== 100)
      $fatal(1, "narrow_written_wide_read was %0d, expected 100",
             narrow_written_wide_read);
    if (wide_written !== 7)
      $fatal(1, "wide_written was %0d, expected 7", wide_written);
    if (absent !== -9)
      $fatal(1, "absent was %0d, expected -9", absent);
    if (count !== 2) $fatal(1, "count was %0d, expected 2", count);
    $display("All checks passed");
  end
endmodule
