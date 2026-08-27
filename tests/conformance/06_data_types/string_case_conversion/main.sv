// toupper and tolower return a new string in which the letters have been
// converted to upper- and lowercase, leaving every character that is not a
// letter as it stands and leaving the string they were called on unchanged
// (LRM 6.16.4, 6.16.5).
module Top;
  string source = "Hello World 123";
  string empty = "";

  string upper;
  string lower;
  string upper_of_upper;
  string upper_of_empty = "unset";
  int upper_of_empty_len = -1;

  initial begin
    upper = source.toupper();
    lower = source.tolower();
    upper_of_upper = upper.toupper();
    upper_of_empty = empty.toupper();
    upper_of_empty_len = upper_of_empty.len();
  end

  final begin
    if (upper != "HELLO WORLD 123")
      $fatal(1, "upper was \"%s\", expected \"HELLO WORLD 123\"", upper);
    if (lower != "hello world 123")
      $fatal(1, "lower was \"%s\", expected \"hello world 123\"", lower);
    if (upper_of_upper != "HELLO WORLD 123")
      $fatal(1, "upper_of_upper was \"%s\", expected \"HELLO WORLD 123\"",
             upper_of_upper);
    if (source != "Hello World 123")
      $fatal(1, "source was \"%s\", expected \"Hello World 123\"", source);
    if (upper_of_empty != "")
      $fatal(1, "upper_of_empty was \"%s\", expected \"\"", upper_of_empty);
    if (upper_of_empty_len !== 0)
      $fatal(1, "upper_of_empty_len was %0d, expected 0", upper_of_empty_len);
    $display("All checks passed");
  end
endmodule
