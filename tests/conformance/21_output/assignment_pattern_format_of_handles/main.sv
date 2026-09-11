// %p prints a class handle or a chandle in an implementation-dependent format,
// except that a null handle prints the word null (LRM 21.2.1.6). What the
// standard fixes is therefore the null spelling and the meaning of the rest:
// the text stands for which object the handle names, so two handles naming one
// object print alike and handles naming different objects do not.
module Top;
  class Cell;
    int v;

    function new(int v);
      this.v = v;
    endfunction
  endclass

  Cell made;
  Cell alias_of_made;
  Cell other;
  Cell empty_handle;
  chandle empty_chandle;

  string pattern;

  string empty_handle_text;
  string empty_chandle_text;
  string made_text;
  string alias_text;
  string other_text;
  string computed_text;

  initial begin
    empty_handle_text = "unset";
    empty_chandle_text = "unset";
    made_text = "unset";
    alias_text = "unset";
    other_text = "unset";
    computed_text = "unset";

    made = new(1);
    other = new(2);
    alias_of_made = made;

    empty_handle_text = $sformatf("%p", empty_handle);
    empty_chandle_text = $sformatf("%p", empty_chandle);
    made_text = $sformatf("%p", made);
    alias_text = $sformatf("%p", alias_of_made);
    other_text = $sformatf("%p", other);

    // A format string the program computes carries its conversions only while
    // it runs (LRM 21.3.3), so the same operand takes the same text by a route
    // that has no directive to read before then.
    pattern = "%p";
    computed_text = $sformatf(pattern, made);
  end

  final begin
    if (empty_handle_text != "null")
      $fatal(1, "empty_handle_text was %s, expected null", empty_handle_text);
    if (empty_chandle_text != "null")
      $fatal(1, "empty_chandle_text was %s, expected null", empty_chandle_text);

    if (made_text == "null")
      $fatal(1, "made_text was null for a handle naming an object");
    if (made_text != alias_text)
      $fatal(1, "made_text was %s and alias_text was %s, expected one text",
             made_text, alias_text);
    if (made_text == other_text)
      $fatal(1, "made_text and other_text were both %s for two objects",
             made_text);
    if (computed_text != made_text)
      $fatal(1, "a computed format gave %s where a literal one gave %s",
             computed_text, made_text);

    $display("All checks passed");
  end
endmodule
