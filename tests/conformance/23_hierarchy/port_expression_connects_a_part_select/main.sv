// A non-ANSI port list may connect a port to a port expression naming part of
// an internal name rather than the whole of it (LRM 23.2.2.2). What crosses
// the port is then that part, so two ports whose expressions select disjoint
// parts of one internal name each carry their own connection into their own
// bits, and neither disturbs the other's. The same holds out of the child, and
// in an ANSI header too, where an explicit port names its expression and no
// declaration of its own (LRM 23.2.2.2): an output port written as a part of an
// internal name carries that part, and follows each change the child makes to
// it.
module Child (.lo(w[3:0]), .hi(w[7:4]));
  input [7:0] w;
endmodule

module Source (output .whole(r), output .lo(r[3:0]), output .hi(r[7:4]));
  logic [7:0] r;
  initial begin
    r = 8'hA5;
    #2 r = 8'h3C;
  end
endmodule

module Top;
  logic [3:0] low, high;
  logic [7:0] joined;

  Child u (.lo(low), .hi(high));

  logic [7:0] whole;
  logic [3:0] out_low, out_high;
  logic [7:0] first_seen, second_seen;

  Source s (.whole(whole), .lo(out_low), .hi(out_high));

  initial begin
    joined = 8'h00;
    low = 4'h5;
    high = 4'hA;
    #1;
    joined = u.w;
    first_seen = {out_high, out_low};
    #2;
    second_seen = {out_high, out_low};
  end

  final begin
    if (joined !== 8'hA5) $fatal(1, "u.w was %0h, expected a5", joined);
    if (first_seen !== 8'hA5)
      $fatal(1, "the output parts read %0h first, expected a5", first_seen);
    if (second_seen !== 8'h3C)
      $fatal(1, "the output parts read %0h after the change, expected 3c",
             second_seen);
    if (whole !== 8'h3C) $fatal(1, "the whole output was %0h", whole);
    $display("All checks passed");
  end
endmodule
