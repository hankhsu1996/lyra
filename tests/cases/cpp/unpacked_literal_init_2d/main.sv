module Top;
  int a [2][3] = '{'{1, 2, 3}, '{4, 5, 6}};
  initial begin
    $display("%0d", a[0][0]);
    $display("%0d", a[0][1]);
    $display("%0d", a[0][2]);
    $display("%0d", a[1][0]);
    $display("%0d", a[1][1]);
    $display("%0d", a[1][2]);
  end
endmodule
