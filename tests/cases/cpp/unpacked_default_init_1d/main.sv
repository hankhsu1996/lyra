module Top;
  int a [3];
  initial begin
    $display("%0d", a[0]);
    $display("%0d", a[1]);
    $display("%0d", a[2]);
  end
endmodule
