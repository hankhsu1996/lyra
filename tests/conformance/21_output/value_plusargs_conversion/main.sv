// @argv: +NUM=42 +HEX=ff +OCT=17 +BIN=1011 +STR=hello +EMPTY=
//
// $value$plusargs matches the plusarg_string portion of its first argument
// against the command line the way $test$plusargs does, then converts what
// remains of the matching plusarg according to the format string that follows
// -- decimal, hexadecimal, octal, binary or string -- and stores the result in
// the variable given. A match returns a nonzero integer, and where the variable
// is wider than the converted value the stored value is zero-padded to the
// variable's width. A match whose remainder is empty still returns nonzero and
// still writes the variable, with a zero or an empty string. Only a failure to
// match leaves the variable as it was, and that is the case that returns zero
// (LRM 21.6).
module Top;
  int matched_num;
  int num;

  int matched_hex;
  logic [15:0] hex;

  int matched_oct;
  int oct;

  int matched_bin;
  logic [3:0] bin;

  int matched_str;
  string str;

  int matched_empty_num;
  int empty_num;

  int matched_empty_str;
  string empty_str;

  int matched_absent;
  int absent;

  int matched_absent_str;
  string absent_str;

  initial begin
    num = -1;
    hex = 16'hFFFF;
    oct = -1;
    bin = 4'h0;
    str = "unset";
    empty_num = 42;
    empty_str = "unset";
    absent = 77;
    absent_str = "kept";
    matched_absent = 1;
    matched_absent_str = 1;

    matched_num = $value$plusargs("NUM=%d", num);
    matched_hex = $value$plusargs("HEX=%h", hex);
    matched_oct = $value$plusargs("OCT=%o", oct);
    matched_bin = $value$plusargs("BIN=%b", bin);
    matched_str = $value$plusargs("STR=%s", str);
    matched_empty_num = $value$plusargs("EMPTY=%d", empty_num);
    matched_empty_str = $value$plusargs("EMPTY=%s", empty_str);
    matched_absent = $value$plusargs("ABSENT=%d", absent);
    matched_absent_str = $value$plusargs("ABSENT=%s", absent_str);
  end

  final begin
    if (matched_num === 0) $fatal(1, "NUM=%%d did not match +NUM=42");
    if (num !== 42) $fatal(1, "num was %0d, expected 42", num);
    if (matched_hex === 0) $fatal(1, "HEX=%%h did not match +HEX=ff");
    if (hex !== 16'h00FF) $fatal(1, "hex was %h, expected 00ff", hex);
    if (matched_oct === 0) $fatal(1, "OCT=%%o did not match +OCT=17");
    if (oct !== 15) $fatal(1, "oct was %0d, expected 15", oct);
    if (matched_bin === 0) $fatal(1, "BIN=%%b did not match +BIN=1011");
    if (bin !== 4'b1011) $fatal(1, "bin was %b, expected 1011", bin);
    if (matched_str === 0) $fatal(1, "STR=%%s did not match +STR=hello");
    if (str != "hello") $fatal(1, "str was %s, expected hello", str);
    if (matched_empty_num === 0)
      $fatal(1, "EMPTY=%%d did not match +EMPTY=");
    if (empty_num !== 0)
      $fatal(1, "empty_num was %0d, expected 0", empty_num);
    if (matched_empty_str === 0)
      $fatal(1, "EMPTY=%%s did not match +EMPTY=");
    if (empty_str != "")
      $fatal(1, "empty_str was %s, expected the empty string", empty_str);
    if (matched_absent !== 0)
      $fatal(1, "ABSENT=%%d returned %0d, expected 0", matched_absent);
    if (absent !== 77) $fatal(1, "absent was %0d, expected 77", absent);
    if (matched_absent_str !== 0)
      $fatal(1, "ABSENT=%%s returned %0d, expected 0", matched_absent_str);
    if (absent_str != "kept")
      $fatal(1, "absent_str was %s, expected kept", absent_str);
    $display("All checks passed");
  end
endmodule
