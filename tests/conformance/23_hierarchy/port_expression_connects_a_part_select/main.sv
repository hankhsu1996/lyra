// A non-ANSI port list may connect a port to a port expression naming part of
// an internal name rather than the whole of it (LRM 23.2.2.2). What crosses
// the port is then that part, so two ports whose expressions select disjoint
// parts of one internal name each carry their own connection into their own
// bits, and neither disturbs the other's.
module Child (.lo(w[3:0]), .hi(w[7:4]));
  input [7:0] w;
endmodule

module Top;
  logic [3:0] low, high;
  logic [7:0] joined;

  Child u (.lo(low), .hi(high));

  initial begin
    joined = 8'h00;
    low = 4'h5;
    high = 4'hA;
    #1;
    joined = u.w;
  end

  final begin
    if (joined !== 8'hA5) $fatal(1, "u.w was %0h, expected a5", joined);
    $display("All checks passed");
  end
endmodule
