// A structure declares as many members as its declaration lists, and one with
// several hundred is a structure like any other: each member is its own
// storage, the whole is assigned member by member, and two are equal where
// every member is (LRM 7.2, 11.4.5).
`define N4(p) p``0, p``1, p``2, p``3
`define N16(p) `N4(p``0), `N4(p``1), `N4(p``2), `N4(p``3)
`define N64(p) `N16(p``0), `N16(p``1), `N16(p``2), `N16(p``3)
`define N320(p) `N64(p``0), `N64(p``1), `N64(p``2), `N64(p``3), `N64(p``4)

module Top;
  // 320 members, `m0000` through `m4333`.
  typedef struct {
    int `N320(m);
  } wide_t;

  wide_t original;
  wide_t copy;

  int first_read;
  int last_read;
  int untouched_read;
  bit equal_after_copy;
  bit equal_after_write;

  initial begin
    original.m0000 = 11;
    original.m4333 = 22;

    copy.m0000 = 99;
    copy.m4333 = 99;
    copy.m2000 = 99;
    copy = original;

    first_read = copy.m0000;
    last_read = copy.m4333;
    untouched_read = copy.m2000;

    equal_after_copy = 1'b0;
    equal_after_copy = (copy == original);

    copy.m4333 = 23;
    equal_after_write = 1'b1;
    equal_after_write = (copy == original);
  end

  final begin
    if (first_read !== 11)
      $fatal(1, "the first member read %0d, expected 11", first_read);
    if (last_read !== 22)
      $fatal(1, "the last member read %0d, expected 22", last_read);
    if (untouched_read !== 0)
      $fatal(1, "a member the source never wrote read %0d, expected 0",
             untouched_read);
    if (equal_after_copy !== 1'b1)
      $fatal(1, "a copy compared unequal to what it was copied from");
    if (equal_after_write !== 1'b0)
      $fatal(1, "two structures differing in their last member compared equal");
    if (original.m4333 !== 22)
      $fatal(1, "a write to the copy reached the original: %0d",
             original.m4333);
    $display("All checks passed");
  end
endmodule
