// The formatted text reaches the output variable under the rules for assigning
// a string literal to a variable. An integral destination takes it
// right-justified, padding its leftmost bits with zeros when it is wider than
// the text and dropping the leftmost characters when it is narrower, while an
// unpacked array of bytes takes it left-justified from the array's left bound
// (LRM 21.3.3, 5.9).
module Top;
  bit [31:0] wider_than_text;
  bit [15:0] narrower_than_text;
  byte letters[0:5];

  initial begin
    $sformat(wider_than_text, "AB");
    $sformat(narrower_than_text, "ABC");
    $swrite(letters, "hi");
  end

  final begin
    if (wider_than_text !== 32'h0000_4142)
      $fatal(1, "a wider destination held %0h, expected 00004142",
             wider_than_text);
    if (narrower_than_text !== 16'h4243)
      $fatal(1, "a narrower destination held %0h, expected 4243",
             narrower_than_text);
    if (letters[0] !== 8'h68)
      $fatal(1, "the array's left bound held %0h, expected 68", letters[0]);
    if (letters[1] !== 8'h69)
      $fatal(1, "the next element held %0h, expected 69", letters[1]);
    $display("All checks passed");
  end
endmodule
