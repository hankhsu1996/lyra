module Top;
  int a [6] = '{10, 20, 30, 40, 50, 60};
  int src [3] = '{77, 88, 99};
  int i = 1;
  initial a[i +: 3] = src;
endmodule
