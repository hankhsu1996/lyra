// An associative array declared with a wildcard index type may be indexed by
// any integral expression. The entry an index names is fixed by its numerical
// value alone: the expression is self-determined and treated as unsigned, and
// leading zeros are removed before the value is used, so two indices of
// different widths that carry the same value name one entry. A string literal
// index becomes a bit vector of the equivalent size, and the entries are
// ordered numerically (LRM 7.8.1, 7.8).
module Top;
  int wild [*];

  int narrow_then_wide;
  int count_after_same_value;
  int distinct_value;
  int count_after_distinct;
  int exists_written;
  int exists_never_written;
  int via_unsigned_pattern;
  int count_after_negative;
  int via_character_code;
  int count_after_literal;
  int count_after_delete;
  int exists_deleted;

  initial begin
    wild[8'd5] = 100;
    wild[16'd5] = 200;
    narrow_then_wide = wild[5];
    count_after_same_value = wild.num();

    wild[300] = 7;
    distinct_value = wild[300];
    count_after_distinct = wild.num();

    exists_written = wild.exists(5);
    exists_never_written = wild.exists(7);

    wild[-1] = 42;
    via_unsigned_pattern = wild[32'hFFFFFFFF];
    count_after_negative = wild.num();

    wild["A"] = 65;
    via_character_code = wild[8'd65];
    count_after_literal = wild.num();

    wild.delete(5);
    count_after_delete = wild.num();
    exists_deleted = wild.exists(5);
  end

  final begin
    if (narrow_then_wide !== 200)
      $fatal(1, "narrow_then_wide was %0d, expected 200", narrow_then_wide);
    if (count_after_same_value !== 1)
      $fatal(1, "count_after_same_value was %0d, expected 1",
             count_after_same_value);
    if (distinct_value !== 7)
      $fatal(1, "distinct_value was %0d, expected 7", distinct_value);
    if (count_after_distinct !== 2)
      $fatal(1, "count_after_distinct was %0d, expected 2",
             count_after_distinct);
    if (exists_written !== 1)
      $fatal(1, "exists_written was %0d, expected 1", exists_written);
    if (exists_never_written !== 0)
      $fatal(1, "exists_never_written was %0d, expected 0",
             exists_never_written);
    if (via_unsigned_pattern !== 42)
      $fatal(1, "via_unsigned_pattern was %0d, expected 42",
             via_unsigned_pattern);
    if (count_after_negative !== 3)
      $fatal(1, "count_after_negative was %0d, expected 3",
             count_after_negative);
    if (via_character_code !== 65)
      $fatal(1, "via_character_code was %0d, expected 65", via_character_code);
    if (count_after_literal !== 4)
      $fatal(1, "count_after_literal was %0d, expected 4", count_after_literal);
    if (count_after_delete !== 3)
      $fatal(1, "count_after_delete was %0d, expected 3", count_after_delete);
    if (exists_deleted !== 0)
      $fatal(1, "exists_deleted was %0d, expected 0", exists_deleted);
    $display("All checks passed");
  end
endmodule
