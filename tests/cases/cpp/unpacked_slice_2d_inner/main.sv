module Top;
  int a [2][5] = '{'{10, 20, 30, 40, 50}, '{60, 70, 80, 90, 100}};
  int b [3];
  int i = 1;
  int q0, q1, q2;
  initial begin
    b = a[i][1 +: 3];
    q0 = b[0];
    q1 = b[1];
    q2 = b[2];
  end
endmodule
