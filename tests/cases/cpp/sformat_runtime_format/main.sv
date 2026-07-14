// A format string whose value is known only at simulation time (LRM 21.3.3
// NOTE: format_string need not be a constant expression). The directives are
// parsed and bound to their operands at run time, so the conversion set is the
// same one a literal format string reaches -- specifiers, modifiers, and a
// string operand alike. LRM 21.3.3 also lets the format string itself be an
// integral or unpacked-array-of-byte value, whose bytes carry the text.
//
// A count mismatch does not stop the run: a directive with no operand left
// contributes nothing, and a surplus operand is ignored.
module Top;
  string fmt;
  byte   fmt_bytes[0:3];

  string from_var;
  string from_expr;
  string with_mods;
  string with_string_arg;
  string from_bytes;
  string too_few;
  string too_many;

  int    a;
  string name;

  initial begin
    a = 42;
    name = "lyra";

    fmt = "a=%0d";
    $sformat(from_var, fmt, a);

    // The format slot is an arbitrary expression, not just a plain variable.
    $sformat(from_expr, {"<", fmt, ">"}, a);

    fmt = "[%5d]";
    $sformat(with_mods, fmt, a);

    fmt = "%s=%0d";
    $sformat(with_string_arg, fmt, name, a);

    // An unpacked byte array carrying the text "h=%h".
    fmt_bytes[0] = "h";
    fmt_bytes[1] = "=";
    fmt_bytes[2] = "%";
    fmt_bytes[3] = "h";
    $sformat(from_bytes, fmt_bytes, a);

    fmt = "x=%0d y=%0d";
    $sformat(too_few, fmt, a);

    fmt = "only=%0d";
    $sformat(too_many, fmt, a, a);
  end
endmodule
