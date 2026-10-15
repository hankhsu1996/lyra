// A union declares as many members as its declaration lists, and one with
// several hundred is a union like any other: every member is reached through
// the one storage, a tagged one carries the tag naming the member it holds
// whichever that is, and a copy carries the storage and the tag across (LRM
// 7.3, 7.3.2, 11.4.5).
`define N4(p) p``0, p``1, p``2, p``3
`define N16(p) `N4(p``0), `N4(p``1), `N4(p``2), `N4(p``3)
`define N64(p) `N16(p``0), `N16(p``1), `N16(p``2), `N16(p``3)
`define N320(p) `N64(p``0), `N64(p``1), `N64(p``2), `N64(p``3), `N64(p``4)

module Top;
  // 320 members each, `m0000` through `m4333`.
  typedef union {
    int `N320(m);
  } wide_t;

  typedef union tagged {
    int `N320(m);
  } tagged_wide_t;

  wide_t original;
  wide_t copy;
  tagged_wide_t tagged_original;
  tagged_wide_t tagged_copy;

  int last_read;
  int copy_read;
  int tagged_copy_read;
  bit equal_after_copy;
  bit tagged_equal_after_copy;
  bit tagged_equal_after_retag;
  bit holds_last;

  initial begin
    original.m4333 = 22;
    last_read = original.m4333;
    copy = original;
    copy_read = copy.m4333;
    equal_after_copy = (copy == original);

    tagged_original = tagged m4333 33;
    tagged_copy = tagged_original;
    tagged_copy_read = tagged_copy.m4333;
    tagged_equal_after_copy = (tagged_copy == tagged_original);
    holds_last = 1'b0;
    if (tagged_copy matches tagged m4333 .v) holds_last = 1'b1;

    tagged_copy = tagged m0000 33;
    tagged_equal_after_retag = 1'b1;
    tagged_equal_after_retag = (tagged_copy == tagged_original);
  end

  final begin
    if (last_read !== 22)
      $fatal(1, "the last member read %0d, expected 22", last_read);
    if (copy_read !== 22)
      $fatal(1, "a copy's last member read %0d, expected 22", copy_read);
    if (equal_after_copy !== 1'b1)
      $fatal(1, "a copy compared unequal to what it was copied from");
    if (tagged_copy_read !== 33)
      $fatal(1, "a tagged copy's last member read %0d, expected 33",
             tagged_copy_read);
    if (tagged_equal_after_copy !== 1'b1)
      $fatal(1, "a tagged copy compared unequal to what it was copied from");
    if (holds_last !== 1'b1)
      $fatal(1, "a tagged copy did not carry the tag of the last member");
    if (tagged_equal_after_retag !== 1'b0)
      $fatal(1, "two tagged unions holding different members compared equal");
    $display("All checks passed");
  end
endmodule
