module Top;
  int a [3] = '{10, 20, 30};
  initial begin
    int x;
    x = a[1];
    $display("%0d", x);
  end
endmodule
