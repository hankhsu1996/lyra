// compare orders two strings the way the C strcmp function does: zero when
// they hold the same characters, negative when the string it was called on
// orders before the other, and positive when it orders after. The sign carries
// the ordering and no particular magnitude is required of the result. A string
// that is a prefix of another orders before it, and case matters because an
// uppercase letter's ASCII code is the lower one. icompare answers the same
// question with upper- and lowercase alike (LRM 6.16.6, 6.16.7).
module Top;
  string apple = "apple";
  string apple_again = "apple";
  string apple_upper = "APPLE";
  string banana = "banana";
  string prefix = "app";

  int same = 999;
  int orders_before;
  int orders_after;
  int prefix_before;
  int upper_before;
  int ignoring_case_same = 999;
  int ignoring_case_before;
  int ignoring_case_after;

  initial begin
    same = apple.compare(apple_again);
    orders_before = apple.compare(banana);
    orders_after = banana.compare(apple);
    prefix_before = prefix.compare(apple);
    upper_before = apple_upper.compare(apple);
    ignoring_case_same = apple.icompare(apple_upper);
    ignoring_case_before = apple_upper.icompare(banana);
    ignoring_case_after = banana.icompare(apple_upper);
  end

  final begin
    if (same !== 0) $fatal(1, "same was %0d, expected 0", same);
    if (orders_before >= 0)
      $fatal(1, "orders_before was %0d, expected a negative result",
             orders_before);
    if (orders_after <= 0)
      $fatal(1, "orders_after was %0d, expected a positive result",
             orders_after);
    if (prefix_before >= 0)
      $fatal(1, "prefix_before was %0d, expected a negative result",
             prefix_before);
    if (upper_before >= 0)
      $fatal(1, "upper_before was %0d, expected a negative result",
             upper_before);
    if (ignoring_case_same !== 0)
      $fatal(1, "ignoring_case_same was %0d, expected 0", ignoring_case_same);
    if (ignoring_case_before >= 0)
      $fatal(1, "ignoring_case_before was %0d, expected a negative result",
             ignoring_case_before);
    if (ignoring_case_after <= 0)
      $fatal(1, "ignoring_case_after was %0d, expected a positive result",
             ignoring_case_after);
    $display("All checks passed");
  end
endmodule
