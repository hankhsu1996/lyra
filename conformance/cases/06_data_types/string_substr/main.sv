// substr(i, j) returns a new string holding the characters from position i
// through position j, both ends included, leaving the string it was called on
// alone. A range the string does not hold yields the empty string rather than
// the part of the range that would fit: i below 0, j before i, or j at or past
// the length. An empty string holds no range at all (LRM 6.16.8).
module Top;
  string s = "Hello";
  string empty = "";

  string middle;
  string head;
  string whole;
  string single;
  int whole_len;

  string reversed = "unset";
  string at_length = "unset";
  string past_end = "unset";
  string negative_start = "unset";
  string from_empty = "unset";
  int at_length_len = -1;

  initial begin
    middle = s.substr(1, 3);
    head = s.substr(0, 1);
    whole = s.substr(0, 4);
    single = s.substr(2, 2);
    whole_len = whole.len();

    reversed = s.substr(3, 1);
    at_length = s.substr(0, 5);
    past_end = s.substr(0, 10);
    negative_start = s.substr(-1, 2);
    from_empty = empty.substr(0, 0);
    at_length_len = at_length.len();
  end

  final begin
    if (middle != "ell")
      $fatal(1, "middle was \"%s\", expected \"ell\"", middle);
    if (head != "He") $fatal(1, "head was \"%s\", expected \"He\"", head);
    if (whole != "Hello")
      $fatal(1, "whole was \"%s\", expected \"Hello\"", whole);
    if (whole_len !== 5)
      $fatal(1, "whole_len was %0d, expected 5", whole_len);
    if (single != "l") $fatal(1, "single was \"%s\", expected \"l\"", single);
    if (s != "Hello") $fatal(1, "s was \"%s\", expected \"Hello\"", s);

    if (reversed != "")
      $fatal(1, "reversed was \"%s\", expected \"\"", reversed);
    if (at_length != "")
      $fatal(1, "at_length was \"%s\", expected \"\"", at_length);
    if (at_length_len !== 0)
      $fatal(1, "at_length_len was %0d, expected 0", at_length_len);
    if (past_end != "")
      $fatal(1, "past_end was \"%s\", expected \"\"", past_end);
    if (negative_start != "")
      $fatal(1, "negative_start was \"%s\", expected \"\"", negative_start);
    if (from_empty != "")
      $fatal(1, "from_empty was \"%s\", expected \"\"", from_empty);
    $display("All checks passed");
  end
endmodule
