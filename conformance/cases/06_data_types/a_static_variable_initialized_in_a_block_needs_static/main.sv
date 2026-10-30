// @rejected:
//
// A variable declared in a static procedural block is static by default, and
// "an explicit static keyword shall be required when an initialization value
// is specified as part of a static variable's declaration", so a conforming
// tool rejects this program (LRM 6.21).
module Top;
  initial begin
    int x = 5;
    $display("the program the standard rejects ran with %0d", x);
  end
endmodule
