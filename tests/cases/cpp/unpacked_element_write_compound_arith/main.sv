module Top;
  int a [3] = '{10, 20, 30};
  initial begin
    a[0] += 5;
    a[1] -= 5;
    a[2] *= 2;
  end
endmodule
