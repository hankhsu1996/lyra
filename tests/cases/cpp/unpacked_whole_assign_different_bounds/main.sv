// LRM 7.6: source/target may differ in range bounds; correspondence is
// left-to-right, so b[1] -> a[7], b[2] -> a[6], ..., b[8] -> a[0].
module Top;
  int a [7:0];
  int b [1:8] = '{11, 22, 33, 44, 55, 66, 77, 88};
  int p_a7, p_a6, p_a5, p_a4, p_a3, p_a2, p_a1, p_a0;
  initial begin
    a = b;
    p_a7 = a[7];
    p_a6 = a[6];
    p_a5 = a[5];
    p_a4 = a[4];
    p_a3 = a[3];
    p_a2 = a[2];
    p_a1 = a[1];
    p_a0 = a[0];
  end
endmodule
