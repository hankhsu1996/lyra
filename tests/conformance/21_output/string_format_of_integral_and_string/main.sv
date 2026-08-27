// %s prints its argument as a string. An integral argument is read as a
// sequence of 8-bit ASCII codes, one character per byte, right-justified so
// that the value's rightmost bit is the last character's least significant
// bit; the argument may equally be of string type (LRM 21.2.1.7).
module Top;
  string word;
  logic [23:0] three_letters;
  bit [31:0] four_letters;

  string word_text;
  string three_letters_text;
  string four_letters_text;

  initial begin
    word = "hello";
    three_letters = 24'h41_42_43;
    four_letters = 32'h54_45_53_54;

    word_text = $sformatf("%s", word);
    three_letters_text = $sformatf("got: %s end", three_letters);
    four_letters_text = $sformatf("[%s]", four_letters);
  end

  final begin
    if (word_text != "hello")
      $fatal(1, "a string operand gave '%s', expected hello", word_text);
    if (three_letters_text != "got: ABC end")
      $fatal(1, "a 24-bit operand gave '%s', expected got: ABC end",
             three_letters_text);
    if (four_letters_text != "[TEST]")
      $fatal(1, "a 32-bit operand gave '%s', expected [TEST]",
             four_letters_text);
    $display("All checks passed");
  end
endmodule
