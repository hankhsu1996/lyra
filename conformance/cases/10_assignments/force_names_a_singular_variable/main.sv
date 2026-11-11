// @rejected:
//
// The left-hand side of a force "can be a reference to a singular variable, a
// net, a constant bit-select of a vector net, a constant part-select of a vector
// net, or a concatenation of these" (LRM 10.6.2), and an unpacked array is an
// aggregate type rather than a singular one (LRM 6.4), so a conforming tool
// rejects this program.
module Top;
  int held[2];
  int source[2] = '{1, 2};

  initial begin
    force held = source;
    #1;
    $display("the program the standard rejects ran with %0d", held[1]);
  end
endmodule
