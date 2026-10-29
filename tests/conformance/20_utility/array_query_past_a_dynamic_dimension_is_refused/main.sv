// @rejected:
//
// An array query called with a dimension greater than 1 is an error where that
// dimension is variable-sized, so a conforming tool rejects this program (LRM
// 20.7.1).
module Top;
  int a[3][][5];

  initial begin
    a[2] = new[4];
    $display("the program the standard rejects ran with %0d", $size(a, 2));
  end
endmodule
