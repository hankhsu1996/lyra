module Top;
  int a [2][5] = '{'{10, 20, 30, 40, 50}, '{60, 70, 80, 90, 100}};
  int b [3];
  int i = 1;
  initial b = a[i][1 +: 3];
endmodule
