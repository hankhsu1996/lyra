// %c displays its argument in ASCII character format, and an argument wider
// than one character contributes the byte its low eight bits hold
// (LRM 21.2.1.1, Table 21-1). The wide form is the standard's own worked
// example: a 32-bit variable holding 101 displays as the letter e
// (LRM 21.2.1).
module Top;
  logic [7:0] upper;
  logic [7:0] lower;
  logic [7:0] digit;
  logic [31:0] rval;

  string upper_text;
  string lower_text;
  string digit_text;
  string rval_text;

  initial begin
    upper = 8'h41;
    lower = 8'h61;
    digit = 8'h30;
    rval = 101;

    upper_text = $sformatf("%c", upper);
    lower_text = $sformatf("%c", lower);
    digit_text = $sformatf("%c", digit);
    rval_text = $sformatf("rval has %c ascii character value", rval);
  end

  final begin
    if (upper_text != "A")
      $fatal(1, "code 8'h41 was '%s', expected A", upper_text);
    if (lower_text != "a")
      $fatal(1, "code 8'h61 was '%s', expected a", lower_text);
    if (digit_text != "0")
      $fatal(1, "code 8'h30 was '%s', expected 0", digit_text);
    if (rval_text != "rval has e ascii character value")
      $fatal(1, "the wide operand gave '%s', expected the letter e", rval_text);
    $display("All checks passed");
  end
endmodule
