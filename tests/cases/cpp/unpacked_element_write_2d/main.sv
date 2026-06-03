module Top;
  int a [2][3];
  initial begin
    int i, j;
    for (i = 0; i < 2; i = i + 1) begin
      for (j = 0; j < 3; j = j + 1) begin
        a[i][j] = (i + 1) * 10 + j;
      end
    end
  end
endmodule
