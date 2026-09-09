// %p prints each singular element an aggregate is traversed down to as that
// element would print standing alone, so an element of an enumeration gives the
// name its type declares for the value, and the base type's rendering for a
// value the type declares no name for -- the same rule an operand written as
// one enumeration answers to (LRM 21.2.1.6).
module Top;
  typedef enum {ON, OFF} switch_e;

  switch_e settings [2];
  switch_e outside [1];

  string settings_text;
  string outside_text;

  initial begin
    settings[0] = ON;
    settings[1] = OFF;
    outside[0] = switch_e'(7);

    settings_text = $sformatf("%p", settings);
    outside_text = $sformatf("%p", outside);
  end

  final begin
    if (settings_text != "'{ON, OFF}")
      $fatal(1, "declared enumeration elements printed as '%s'", settings_text);
    if (outside_text != "'{7}")
      $fatal(1, "an element outside the enumeration printed as '%s'",
             outside_text);
    $display("All checks passed");
  end
endmodule
