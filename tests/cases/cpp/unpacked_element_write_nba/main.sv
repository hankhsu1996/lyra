module Top;
  int a [3] = '{1, 2, 3};
  initial begin
    a[0] <= 100;
    a[1] <= 200;
    a[2] <= 300;
    #1;
  end
endmodule
