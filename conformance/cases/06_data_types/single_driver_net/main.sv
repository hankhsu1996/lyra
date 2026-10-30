// A uwire net is an unresolved wire: it admits a single driver, so what it
// carries is that driver's value with nothing to resolve it against, and a
// uwire no driver reaches is high-impedance like any other net (LRM 6.6.2,
// 6.7.1).
module Top;
  logic [7:0] source;

  uwire [7:0] single;
  uwire [7:0] undriven;
  uwire scalar;

  assign single = source;
  assign scalar = source[0];

  logic [7:0] seen_single;
  logic [7:0] seen_undriven;
  logic seen_scalar;

  initial begin
    source = 8'hC3;
    #1;
    seen_single = single;
    seen_undriven = undriven;
    seen_scalar = scalar;
  end

  final begin
    if (seen_single !== 8'hC3)
      $fatal(1, "seen_single was %h, expected c3", seen_single);
    if (seen_undriven !== 8'bzzzzzzzz)
      $fatal(1, "seen_undriven was %b, expected zzzzzzzz", seen_undriven);
    if (seen_scalar !== 1'b1)
      $fatal(1, "seen_scalar was %b, expected 1", seen_scalar);
    $display("All checks passed");
  end
endmodule
