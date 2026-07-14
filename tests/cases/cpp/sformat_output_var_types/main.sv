// $sformat / $swrite output_var of integral and unpacked-byte-array type (LRM
// 21.3.3). The formatted text reaches output_var under the LRM 5.9 string
// literal assignment rules. An integral destination takes it right-justified:
// a destination wider than the text pads its leftmost bits with zeros, and a
// narrower one truncates the leftmost characters. An unpacked byte array takes
// it left-justified, the first character at the left bound, and an element past
// the end of the text keeps the element default.
module Top;
  bit [31:0] wide;
  bit [15:0] narrow;
  byte       bytes[0:5];

  initial begin
    $sformat(wide, "AB");
    $sformat(narrow, "ABC");
    $swrite(bytes, "hi");
  end
endmodule
