// $sformatf formats as $sformat does but hands the text back as its return
// value rather than writing it into an argument, so a call to it stands
// wherever a string value is valid: the right side of an assignment, an
// operand of a comparison, or an argument to another call (LRM 21.3.3).
module Top;
  int count;
  string assigned;
  string nested;
  int compared_equal;

  initial begin
    count = 42;
    assigned = $sformatf("x=%0d", count);
    nested = $sformatf("[%s]", $sformatf("%0h", count));
    if ($sformatf("%0d", count) == "42") compared_equal = 1;
    else compared_equal = 0;
  end

  final begin
    if (assigned != "x=42")
      $fatal(1, "the assigned result was '%s', expected x=42", assigned);
    if (nested != "[2a]")
      $fatal(1, "the nested result was '%s', expected [2a]", nested);
    if (compared_equal !== 1)
      $fatal(1, "the result did not compare equal to the text it holds");
    $display("All checks passed");
  end
endmodule
