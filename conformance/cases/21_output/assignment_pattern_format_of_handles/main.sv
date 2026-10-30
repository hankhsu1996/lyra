// @includes: assignment_pattern.sv
//
// %p prints a class handle or a chandle in an implementation-dependent format,
// except that a null handle prints the word null (LRM 21.2.1.6). The null
// spelling is therefore all the standard fixes of the text, whether the format
// is written in the call or computed while it runs (LRM 21.3.3).
module Top;
  class Cell;
    int v;
  endclass

  Cell empty_handle;
  chandle empty_chandle;

  string pattern;

  string empty_handle_text;
  string empty_chandle_text;
  string computed_text;

  initial begin
    empty_handle_text = "unset";
    empty_chandle_text = "unset";
    computed_text = "unset";

    empty_handle_text = $sformatf("%p", empty_handle);
    empty_chandle_text = $sformatf("%p", empty_chandle);

    pattern = "%p";
    computed_text = $sformatf(pattern, empty_handle);
  end

  final begin
    if (!assignment_pattern::Spells(empty_handle_text, "null"))
      $fatal(1, "empty_handle_text was %s, expected null", empty_handle_text);
    if (!assignment_pattern::Spells(empty_chandle_text, "null"))
      $fatal(1, "empty_chandle_text was %s, expected null", empty_chandle_text);
    if (!assignment_pattern::Spells(computed_text, "null"))
      $fatal(1, "a computed format gave %s for a null handle, expected null",
             computed_text);
    $display("All checks passed");
  end
endmodule
