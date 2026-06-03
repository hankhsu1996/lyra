module Top;
  int a [3] = '{1, 2, 3};
  int b [3] = '{70, 80, 90};
  initial begin
    a <= b;
    #1;
  end
endmodule
