// The expressions of an argument list are evaluated in an order the standard
// calls undefined (LRM 13.5), and an input string crosses to the foreign side
// as the value its expression has (LRM 35.5.6). Lyra evaluates the arguments
// first to last, so a string argument is the value its variable held before a
// later argument changes the variable: the foreign side reads that value and
// not the one written after it, and not storage the write let go of.
module Top;
  import "DPI-C" function int first_char(input string a, input int b);

  string text = "a";

  function automatic int lengthen();
    text = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz";
    return 0;
  endfunction

  initial begin
    int seen;
    seen = first_char(text, lengthen());
    if (seen != 97)
      $fatal(1, "the string argument began with %0d, expected 97", seen);
    if (text.len() != 66)
      $fatal(1, "the later argument left the string %0d long, expected 66",
             text.len());
    $display("All checks passed");
  end
endmodule
