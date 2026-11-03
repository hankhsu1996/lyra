// LRM 21.2.1.1 gives a `-` before the field width only to the real
// conversions, which have C's whole formatting capability, and 21.2.1.2 pads
// every other field on the left: decimal and string values with spaces, and
// hexadecimal, octal and binary values with leading zeros, which "are always
// displayed". The front end accepts a `-` on every conversion that takes a
// width, and what it means there the standard does not say. Lyra puts a
// field's spaces on the right, decimal, string and time alike, and keeps a
// radix's leading zeros on the left, whether the width is written or comes
// from the operand's type; a real field is justified as C does it, a `-`
// overriding a `0`. A string formatted while the design is elaborated is the
// same as one formatted while it runs. This case holds Lyra to that.
module Top;
  localparam string Elaborated = $sformatf(
      "[%-6d][%-4s][%-6h][%-6o][%-6b][%-d][%-h][%-b][%-4t][%-08.1f]",
      8'd5, "a", 8'h1, 8'o7, 3'b101, 8'd5, 8'h1, 4'b1, 0, 1.5);

  initial begin
    string run;
    run = $sformatf(
        "[%-6d][%-4s][%-6h][%-6o][%-6b][%-d][%-h][%-b][%-4t][%-08.1f]",
        8'd5, "a", 8'h1, 8'o7, 3'b101, 8'd5, 8'h1, 4'b1, 0, 1.5);

    if (run != "[5     ][a   ][000001][000007][000101][5  ][01][0001][0   ][1.5     ]")
      $fatal(1, "formatted while running gave '%s'", run);
    if (Elaborated != run)
      $fatal(1, "formatted while elaborating gave '%s', running gave '%s'",
             Elaborated, run);
    $display("All checks passed");
  end
endmodule
