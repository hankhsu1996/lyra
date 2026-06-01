module Top;
  int a [2][3] = '{'{1, 2, 3}, '{4, 5, 6}};
  initial begin
    int x;
    x = a[1][1];
    $display("%0d", x);
  end
endmodule
