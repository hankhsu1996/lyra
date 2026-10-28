// @reports: error
//
// Unpacking through a `with` range that reaches past the bounds of a
// fixed-size array is an error, raised when the range is evaluated, just
// before the array is unpacked (LRM 11.4.14.4).
module Top;
  byte pair[2];

  initial begin
    int count;
    count = 3;
    $display("Ready for the report");
    {>>{pair with [0 +: count]}} = 24'h010203;
    $display("All checks passed");
  end
endmodule
