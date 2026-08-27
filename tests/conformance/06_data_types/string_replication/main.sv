// A replication with at least one operand of string type, or with a multiplier
// that is not constant, converts every string literal among its operands to
// string type and yields a string holding that many copies of the inner
// concatenation. The multiplier is an ordinary non-negative integral
// expression rather than a constant one, and a multiplier of zero yields the
// empty string (LRM 6.16, Table 6-9).
module Top;
  string base = "abc";
  string a = "a";
  string b = "b";
  int times;

  string constant_multiplier;
  string runtime_multiplier;
  string once;
  string none = "unset";
  int none_len = -1;
  string pair_repeated;
  string literal_with_runtime_multiplier;

  initial begin
    constant_multiplier = {3{base}};

    times = 4;
    runtime_multiplier = {times{base}};

    times = 1;
    once = {times{base}};

    times = 0;
    none = {times{base}};
    none_len = none.len();

    pair_repeated = {4{a, b}};

    times = 3;
    literal_with_runtime_multiplier = {times{"xy"}};
  end

  final begin
    if (constant_multiplier != "abcabcabc")
      $fatal(1, "constant_multiplier was \"%s\", expected \"abcabcabc\"",
             constant_multiplier);
    if (runtime_multiplier != "abcabcabcabc")
      $fatal(1, "runtime_multiplier was \"%s\", expected \"abcabcabcabc\"",
             runtime_multiplier);
    if (once != "abc") $fatal(1, "once was \"%s\", expected \"abc\"", once);
    if (none != "") $fatal(1, "none was \"%s\", expected \"\"", none);
    if (none_len !== 0) $fatal(1, "none_len was %0d, expected 0", none_len);
    if (pair_repeated != "abababab")
      $fatal(1, "pair_repeated was \"%s\", expected \"abababab\"",
             pair_repeated);
    if (literal_with_runtime_multiplier != "xyxyxy")
      $fatal(1,
             "literal_with_runtime_multiplier was \"%s\", expected \"xyxyxy\"",
             literal_with_runtime_multiplier);
    $display("All checks passed");
  end
endmodule
