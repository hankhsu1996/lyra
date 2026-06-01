module Top;
  int a [3] = '{10, 20, 30};
  initial begin
    $display("%0d", a[0]);
    $display("%0d", a[1]);
    $display("%0d", a[2]);
  end
endmodule
