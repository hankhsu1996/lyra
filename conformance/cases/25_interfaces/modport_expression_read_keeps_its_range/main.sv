// The self-determined type of a modport expression becomes the type of the
// port identifier it is named with (LRM 25.5.4), so a module reading the
// identifier through the view reads the expression's value as that type: a
// select into it at the bound a query reports (LRM 20.7) reads the bit at that
// end, and the width is the expression's own (LRM 11.6.1).
interface Bytes;
  logic [7:0] a;
  logic [7:0] b;

  modport view(input .Hi(a[7:4]), .Sum(a[7:4] + b[7:4]));
endinterface

module Reader (
    Bytes.view i
);
  logic [3:0] hi = 4'hx;
  logic [3:0] sum = 4'hx;
  logic [3:0] ends = 4'hx;
  int widths = -1;

  initial #1 begin
    hi = i.Hi;
    sum = i.Sum;
    ends = {i.Hi[$left(i.Hi)], i.Hi[$right(i.Hi)],
            i.Sum[$left(i.Sum)], i.Sum[$right(i.Sum)]};
    widths = $bits(i.Hi) * 10 + $bits(i.Sum);
  end
endmodule

module Top;
  Bytes bytes ();

  Reader reader (.i(bytes.view));

  initial begin
    bytes.a = 8'hA6;
    bytes.b = 8'h40;
  end

  final begin
    if (reader.hi !== 4'hA) $fatal(1, "reader.hi was %h, expected a", reader.hi);
    // 4'hA + 4'h4.
    if (reader.sum !== 4'hE)
      $fatal(1, "reader.sum was %h, expected e", reader.sum);
    // 4'b1010 and 4'b1110, each read at its left and its right end.
    if (reader.ends !== 4'b1010)
      $fatal(1, "the ends read were %b, expected 1010", reader.ends);
    if (reader.widths !== 44)
      $fatal(1, "the widths read were %0d, expected 44", reader.widths);
    $display("All checks passed");
  end
endmodule
