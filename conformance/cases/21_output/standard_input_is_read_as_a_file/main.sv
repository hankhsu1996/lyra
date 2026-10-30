// STDIN is pre-opened for reading (LRM 21.3.1), so every read a file
// descriptor takes reads it (LRM 21.3.4): a character, a character pushed
// back, a line, and a scan, each starting where the one before stopped. A scan
// leaves the character that ended it unread (LRM 21.3.4.3), and once the input
// is exhausted a scan answers EOF and $feof says so (LRM 21.3.8).
//
// Standard input here is a pipe, which cannot seek, so nothing can be read
// ahead and given back: each read takes from the stream what it uses.
module Top;
  import "DPI-C" function int feed_standard_input(string text);

  localparam int STDIN = 32'h8000_0000;

  int fed;
  int first;
  int pushed;
  int again;
  int line_length;
  string line;
  int scanned_pair;
  int a;
  int b;
  int scanned_word;
  string word;
  int scanned_past_end;
  int past_end_value;
  int at_end;

  initial begin
    fed = feed_standard_input("Aline two\n42 7\nend");
    first = $fgetc(STDIN);
    pushed = $ungetc(first, STDIN);
    again = $fgetc(STDIN);
    line_length = $fgets(line, STDIN);
    scanned_pair = $fscanf(STDIN, "%d %d", a, b);
    scanned_word = $fscanf(STDIN, "%s", word);
    past_end_value = -5;
    scanned_past_end = $fscanf(STDIN, "%d", past_end_value);
    at_end = $feof(STDIN);
  end

  final begin
    if (fed !== 1) $fatal(1, "standard input could not be set up");
    if (first !== 65)
      $fatal(1, "the first character was %0d, expected 65", first);
    if (pushed !== 0)
      $fatal(1, "pushing the character back answered %0d, expected 0", pushed);
    if (again !== 65)
      $fatal(1, "the character read after pushing back was %0d, expected 65",
             again);
    if (line_length !== 9 || line != "line two\n")
      $fatal(1, "the line read was %0d characters '%s', expected 'line two'",
             line_length, line);
    if (scanned_pair !== 2 || a !== 42 || b !== 7)
      $fatal(1, "the scan of two numbers answered %0d with %0d and %0d",
             scanned_pair, a, b);
    if (scanned_word !== 1 || word != "end")
      $fatal(1, "the scan of a word answered %0d with '%s', expected 'end'",
             scanned_word, word);
    if (scanned_past_end !== -1)
      $fatal(1, "a scan past the end answered %0d, expected -1",
             scanned_past_end);
    if (past_end_value !== -5)
      $fatal(1, "a scan past the end wrote %0d into its output",
             past_end_value);
    if (at_end == 0) $fatal(1, "$feof did not report the end of input");
    $display("All checks passed");
  end
endmodule
