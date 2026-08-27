// %p prints a singular value as it would stand as one element of an assignment
// pattern: a string enclosed in quotes, an enumerated value as its enumeration
// name when the value is one the type declares and by its base type when it is
// not, and any other singular value as it would print with no format
// specification at all -- which for this family of tasks is decimal
// (LRM 21.2.1.6, 21.2.1.1).
module Top;
  typedef enum {ON, OFF} switch_e;

  string word;
  bit [7:0] byte_value;
  logic [11:0] full_value;
  switch_e undeclared;

  string word_text;
  string byte_value_text;
  string full_value_text;
  string undeclared_text;

  switch_e declared;
  string declared_text;

  initial begin
    word = "hello";
    byte_value = 8'hAA;
    full_value = 12'd4095;
    undeclared = switch_e'(7);

    word_text = $sformatf("%p", word);
    byte_value_text = $sformatf("%p", byte_value);
    full_value_text = $sformatf("%p", full_value);
    undeclared_text = $sformatf("%p", undeclared);

    declared = ON;
    declared_text = $sformatf("%p", declared);
  end

  final begin
    if (word_text != "\"hello\"")
      $fatal(1, "a string was printed as %s, expected it in quotes",
             word_text);
    if (byte_value_text != "170")
      $fatal(1, "8'hAA was printed as '%s', expected 170", byte_value_text);
    if (full_value_text != "4095")
      $fatal(1, "12'd4095 was printed as '%s', expected 4095",
             full_value_text);
    if (undeclared_text != "7")
      $fatal(1, "a value outside the enumeration gave '%s', expected 7",
             undeclared_text);

    if (declared_text != "ON")
      $fatal(1, "a declared enumeration value gave '%s', expected ON",
             declared_text);
    $display("All checks passed");
  end
endmodule
