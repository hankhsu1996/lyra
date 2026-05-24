module Top;
  int x;
  int y;
  initial begin
    x = 1;
    $finish;
    x = 2;
  end
  final begin
    y = 99;
  end
endmodule
