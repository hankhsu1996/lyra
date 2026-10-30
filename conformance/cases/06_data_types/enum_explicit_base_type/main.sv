// An enumeration declared without a data type has base type int, so its values
// are thirty-two bits wide and signed. Any other base type has to be written
// into the declaration, and the enumeration's values then have the width of
// the type named there (LRM 6.19).
module Top;
  typedef enum {ZERO, ONE, NEG = -1} default_t;
  typedef enum logic [2:0] {LOW, MID, HIGH} narrow_t;

  default_t d;
  narrow_t n;
  longint widened;
  int default_bits;
  int narrow_bits;
  int high_value;

  initial begin
    d = NEG;
    widened = d;
    default_bits = $bits(default_t);

    n = HIGH;
    high_value = n;
    narrow_bits = $bits(narrow_t);
  end

  final begin
    if (default_bits !== 32)
      $fatal(1, "default_bits was %0d, expected 32", default_bits);
    if (widened !== -1) $fatal(1, "widened was %0d, expected -1", widened);
    if (narrow_bits !== 3)
      $fatal(1, "narrow_bits was %0d, expected 3", narrow_bits);
    if (high_value !== 2)
      $fatal(1, "high_value was %0d, expected 2", high_value);
    $display("All checks passed");
  end
endmodule
