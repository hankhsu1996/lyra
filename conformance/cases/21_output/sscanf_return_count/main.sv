// $sscanf returns the number of input items it matched and assigned, which is
// 0 when the very first thing it tries fails to match, and EOF when the input
// ends before any matching failure or conversion. White space in the control
// string matches any run of white space in the input, an ordinary character
// must match itself, and %% matches one %. An output argument no conversion
// reached keeps the value it already held (LRM 21.3.4.3).
module Top;
  int both;
  int first_of_two;
  int second_of_two;

  int partial;
  int matched_value;
  int unreached_value;

  int at_end;
  int untouched_by_end;

  int no_match;
  int untouched_by_mismatch;

  int up_to_literal;
  int before_literal;
  int after_literal;

  int across_white_space;
  int first_spaced;
  int second_spaced;
  int third_spaced;

  int with_percent;
  int percent_value;

  initial begin
    both = $sscanf("12 34", "%d %d", first_of_two, second_of_two);

    unreached_value = 99;
    partial = $sscanf("12 abc", "%d %d", matched_value, unreached_value);

    untouched_by_end = 42;
    at_end = $sscanf("", "%d", untouched_by_end);

    untouched_by_mismatch = 7;
    no_match = $sscanf("abc", "%d", untouched_by_mismatch);

    after_literal = 99;
    up_to_literal = $sscanf("12x34", "%d:%d", before_literal, after_literal);

    across_white_space = $sscanf("1\t2\n3", "%d %d %d", first_spaced,
                                 second_spaced, third_spaced);

    with_percent = $sscanf("100%", "%d%%", percent_value);
  end

  final begin
    if (both !== 2)
      $fatal(1, "two matching conversions returned %0d, expected 2", both);
    if (first_of_two !== 12 || second_of_two !== 34)
      $fatal(1, "the two values were %0d and %0d, expected 12 and 34",
             first_of_two, second_of_two);

    if (partial !== 1)
      $fatal(1, "one of two conversions matching returned %0d, expected 1",
             partial);
    if (matched_value !== 12)
      $fatal(1, "the matched value was %0d, expected 12", matched_value);
    if (unreached_value !== 99)
      $fatal(1, "the unreached output was %0d, expected the 99 it held",
             unreached_value);

    if (at_end !== -1)
      $fatal(1, "an empty input returned %0d, expected -1", at_end);
    if (untouched_by_end !== 42)
      $fatal(1, "the output after an empty input was %0d, expected 42",
             untouched_by_end);

    if (no_match !== 0)
      $fatal(1, "a failure before any conversion returned %0d, expected 0",
             no_match);
    if (untouched_by_mismatch !== 7)
      $fatal(1, "the output after a failed first item was %0d, expected 7",
             untouched_by_mismatch);

    if (up_to_literal !== 1)
      $fatal(1, "an unmatched literal returned %0d, expected 1",
             up_to_literal);
    if (before_literal !== 12)
      $fatal(1, "the value before the literal was %0d, expected 12",
             before_literal);
    if (after_literal !== 99)
      $fatal(1, "the value after the unmatched literal was %0d, expected 99",
             after_literal);

    if (across_white_space !== 3)
      $fatal(1, "three conversions across white space returned %0d",
             across_white_space);
    if (first_spaced !== 1 || second_spaced !== 2 || third_spaced !== 3)
      $fatal(1, "the spaced values were %0d %0d %0d, expected 1 2 3",
             first_spaced, second_spaced, third_spaced);

    if (with_percent !== 1)
      $fatal(1, "a control string ending in %%%% returned %0d, expected 1",
             with_percent);
    if (percent_value !== 100)
      $fatal(1, "the value before the %%%% was %0d, expected 100",
             percent_value);
    $display("All checks passed");
  end
endmodule
