module Top;
  parameter int N = 5;
  int x;
  int y;
  initial begin
    x = N + 1;
    if (N > 0) y = N * 2;
    else y = -1;
  end
endmodule
