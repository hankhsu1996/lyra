// $sformat writes its formatted text into its first argument and reads its
// second argument, and only its second, as the format string -- so a later
// argument that happens to contain a conversion is text like any other. Every
// format specifier the display tasks support is available to it (LRM 21.3.3,
// 21.2.1.1).
module Top;
  int count;
  logic [15:0] wide_value;
  logic [7:0] octal_value;
  logic [3:0] nibble;
  string greeting;

  string specifiers;
  string strings;
  string in_a_field;
  string not_a_format;

  initial begin
    count = 42;
    wide_value = 16'hDEAD;
    octal_value = 8'o377;
    nibble = 4'b1011;
    greeting = "world";

    $sformat(specifiers, "d=%0d h=%0h o=%0o b=%0b", count, wide_value,
             octal_value, nibble);
    $sformat(strings, "hi=%s lit=%s", greeting, "literal");
    $sformat(in_a_field, "[%5d]", 7);
    $sformat(not_a_format, "%s", "%0d");
  end

  final begin
    if (specifiers != "d=42 h=dead o=377 b=1011")
      $fatal(1, "the conversions gave '%s', expected d=42 h=dead o=377 b=1011",
             specifiers);
    if (strings != "hi=world lit=literal")
      $fatal(1,
             "the string conversions gave '%s', expected hi=world lit=literal",
             strings);
    if (in_a_field != "[    7]")
      $fatal(1, "7 in a field of five gave '%s', expected four spaces then 7",
             in_a_field);
    if (not_a_format != "%0d")
      $fatal(1, "an argument holding a conversion gave '%s', expected text",
             not_a_format);
    $display("All checks passed");
  end
endmodule
