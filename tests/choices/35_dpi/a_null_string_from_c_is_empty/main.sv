// A string crosses from C to SystemVerilog as a `const char*`, and the
// standard asks the foreign side to hand over a properly null-terminated C
// string (LRM Annex H.8.10); a null pointer is no C string, and the standard
// says nothing of what it reads as. Lyra reads it as the empty string, wherever
// the string crosses: as an import's result, through an output and an inout
// formal, and as an argument foreign code hands an export (LRM 35.5.6). The
// UVM library's own C returns a null pointer to say "no more arguments" and
// reads that back as the empty string.
module Top;
  import "DPI-C" function string no_text();
  import "DPI-C" function void no_text_out(output string s);
  import "DPI-C" function void no_text_inout(inout string s);
  import "DPI-C" context function void hand_no_text();
  export "DPI-C" function take_text;

  string taken = "replaced";

  function void take_text(input string s);
    taken = s;
  endfunction

  initial begin
    string got;
    string changed = "replaced";
    got = no_text();
    if (got != "" || got.len() != 0)
      $fatal(1, "a null result read as \"%s\", expected the empty string", got);
    got = "replaced";
    no_text_out(got);
    if (got != "" || got.len() != 0)
      $fatal(1, "a null output read as \"%s\", expected the empty string", got);
    no_text_inout(changed);
    if (changed != "" || changed.len() != 0)
      $fatal(1, "a null inout read as \"%s\", expected the empty string",
             changed);
    hand_no_text();
    if (taken != "" || taken.len() != 0)
      $fatal(1, "a null argument to an export read as \"%s\", expected \"\"",
             taken);
    $display("All checks passed");
  end
endmodule
