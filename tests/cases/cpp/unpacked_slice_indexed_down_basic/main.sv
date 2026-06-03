module Top;
  int a [6] = '{10, 20, 30, 40, 50, 60};
  int b [3];
  int i = 4;
  initial b = a[i -: 3];
endmodule
