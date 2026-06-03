// LRM 7.6: source/target may differ in range bounds; correspondence is
// left-to-right, so b[1] -> a[7], b[2] -> a[6], ..., b[8] -> a[0].
module Top;
  int a [7:0];
  int b [1:8] = '{11, 22, 33, 44, 55, 66, 77, 88};
  initial a = b;
endmodule
