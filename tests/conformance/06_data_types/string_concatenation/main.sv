// A concatenation with at least one operand of string type converts every
// string literal among its operands to string type first, and its result is a
// string holding the operands' characters end to end. A string holding no
// characters contributes none, so it leaves the result's length alone. When
// every operand is a string literal the concatenation is one of integral
// values instead: "" is then the byte 0, which an integral target keeps and a
// string target drops along with every other "\0" (LRM 6.16, Table 6-9).
module Top;
  string first = "one";
  string second = "two";
  string third = "three";
  string empty = "";

  string two_variables;
  string variable_and_literal;
  string separated;
  string inner;
  string nested;
  string with_empty;
  int with_empty_len;
  string all_literals;
  string literal_empty_in_string;
  int literal_empty_in_string_len;
  logic [15:0] literal_empty_in_packed;

  initial begin
    two_variables = {first, second};
    variable_and_literal = {first, " there"};
    separated = {first, "-", second, "-", third};
    inner = {second, "-", third};
    nested = {first, "-", inner};

    with_empty = {"prefix", empty, "suffix"};
    with_empty_len = with_empty.len();

    all_literals = {"foo", "bar"};
    literal_empty_in_string = {"H", ""};
    literal_empty_in_string_len = literal_empty_in_string.len();
    literal_empty_in_packed = {"H", ""};
  end

  final begin
    if (two_variables != "onetwo")
      $fatal(1, "two_variables was \"%s\", expected \"onetwo\"",
             two_variables);
    if (variable_and_literal != "one there")
      $fatal(1, "variable_and_literal was \"%s\", expected \"one there\"",
             variable_and_literal);
    if (separated != "one-two-three")
      $fatal(1, "separated was \"%s\", expected \"one-two-three\"", separated);
    if (nested != "one-two-three")
      $fatal(1, "nested was \"%s\", expected \"one-two-three\"", nested);

    if (with_empty != "prefixsuffix")
      $fatal(1, "with_empty was \"%s\", expected \"prefixsuffix\"",
             with_empty);
    if (with_empty_len !== 12)
      $fatal(1, "with_empty_len was %0d, expected 12", with_empty_len);

    if (all_literals != "foobar")
      $fatal(1, "all_literals was \"%s\", expected \"foobar\"", all_literals);
    if (literal_empty_in_string != "H")
      $fatal(1, "literal_empty_in_string was \"%s\", expected \"H\"",
             literal_empty_in_string);
    if (literal_empty_in_string_len !== 1)
      $fatal(1, "literal_empty_in_string_len was %0d, expected 1",
             literal_empty_in_string_len);
    if (literal_empty_in_packed !== 16'h4800)
      $fatal(1, "literal_empty_in_packed was %h, expected 4800",
             literal_empty_in_packed);
    $display("All checks passed");
  end
endmodule
