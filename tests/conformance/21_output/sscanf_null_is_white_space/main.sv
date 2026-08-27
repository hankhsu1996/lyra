// For $sscanf a null character counts as white space, so it ends an input
// field the way a blank does, and a conversion that follows skips over it. The
// sources are unpacked byte arrays because that is how a null byte reaches the
// function without being the end of a string (LRM 21.3.4.3).
module Top;
  byte numbers[0:6];
  byte words[0:10];

  int number_count;
  int first_number;
  int second_number;

  int word_count;
  string first_word;
  string second_word;

  initial begin
    // "42", a null, then "9900".
    numbers = '{8'h34, 8'h32, 8'h00, 8'h39, 8'h39, 8'h30, 8'h30};
    // "hello", a null, then "world".
    words = '{8'h68, 8'h65, 8'h6C, 8'h6C, 8'h6F,
              8'h00,
              8'h77, 8'h6F, 8'h72, 8'h6C, 8'h64};

    number_count = $sscanf(numbers, "%d %d", first_number, second_number);
    word_count = $sscanf(words, "%s %s", first_word, second_word);
  end

  final begin
    if (number_count !== 2)
      $fatal(1, "two numbers around a null returned %0d, expected 2",
             number_count);
    if (first_number !== 42)
      $fatal(1, "the number before the null was %0d, expected 42",
             first_number);
    if (second_number !== 9900)
      $fatal(1, "the number after the null was %0d, expected 9900",
             second_number);

    if (word_count !== 2)
      $fatal(1, "two words around a null returned %0d, expected 2",
             word_count);
    if (first_word != "hello")
      $fatal(1, "the word before the null was '%s', expected 'hello'",
             first_word);
    if (second_word != "world")
      $fatal(1, "the word after the null was '%s', expected 'world'",
             second_word);
    $display("All checks passed");
  end
endmodule
