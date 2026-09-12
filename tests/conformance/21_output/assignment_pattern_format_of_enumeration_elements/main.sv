// %p prints each singular element an aggregate is traversed down to as that
// element would print standing alone, so an element of an enumeration gives the
// name its type declares for the value, and the base type's rendering for a
// value the type declares no name for -- the same rule an operand written as
// one enumeration answers to (LRM 21.2.1.6). The traversal reaches an element
// through every kind of aggregate, including a packed one, where the element is
// a run of bits rather than storage of its own.
module Top;
  typedef enum {ON, OFF} switch_e;

  typedef struct {
    switch_e sw;
    int count;
  } setting_t;

  typedef struct packed {
    switch_e sw;
    bit [3:0] pad;
  } packed_setting_t;

  switch_e settings [2];
  switch_e outside [1];
  setting_t nested;
  packed_setting_t packed_nested;

  string settings_text = "unset";
  string outside_text = "unset";
  string nested_text = "unset";
  string packed_nested_text = "unset";

  initial begin
    settings[0] = ON;
    settings[1] = OFF;
    outside[0] = switch_e'(7);
    nested = '{sw: OFF, count: 3};
    packed_nested = '{sw: ON, pad: 4'd5};

    settings_text = $sformatf("%p", settings);
    outside_text = $sformatf("%p", outside);
    nested_text = $sformatf("%p", nested);
    packed_nested_text = $sformatf("%p", packed_nested);
  end

  final begin
    if (settings_text != "'{ON, OFF}")
      $fatal(1, "declared enumeration elements printed as '%s'", settings_text);
    if (outside_text != "'{7}")
      $fatal(1, "an element outside the enumeration printed as '%s'",
             outside_text);
    if (nested_text != "'{sw:OFF, count:3}")
      $fatal(1, "an enumeration member of a structure printed as '%s'",
             nested_text);
    if (packed_nested_text != "'{sw:ON, pad:5}")
      $fatal(1, "an enumeration member of a packed structure printed as '%s'",
             packed_nested_text);
    $display("All checks passed");
  end
endmodule
