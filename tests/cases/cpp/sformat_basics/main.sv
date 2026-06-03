module Top;
  // LRM 21.3.3: $sformat writes its formatted output to the first argument
  // (a string-typed lvalue per Cut 1 scope). Format conversions reuse the
  // same engine as $display, so this case only proves end-to-end wiring
  // across a representative spec set plus the width / precision / pad /
  // align modifiers rather than re-testing every spec semantics.
  int          a;
  logic [15:0] b;
  logic [7:0]  c;
  logic [3:0]  d;
  int          n;
  string       greeting;
  string       label;
  string       out_specs;
  string       out_strings;
  string       out_width;
  string       out_lalign;
  string       out_zpad;
  initial begin
    a = 42;
    b = 16'hDEAD;
    c = 8'o377;
    d = 4'b1011;
    n = 7;
    greeting = "world";
    label = "hi";
    $sformat(out_specs, "d=%0d h=%0h o=%0o b=%0b", a, b, c, d);
    // %s consumes both a `string` arg and a packed-string-literal arg --
    // proves the implicit ConversionExpr path the print engine inserts
    // for non-string %s operands also fires through the sformat lowering.
    $sformat(out_strings, "hi=%s lit=%s", greeting, "literal");
    // Format-modifier wiring: width / left-align / zero-pad all feed the
    // same FormatModifiers struct the print engine uses.
    $sformat(out_width, "[%5d]", n);
    $sformat(out_lalign, "[%-5s]", label);
    $sformat(out_zpad, "[%04h]", n);
  end
endmodule
