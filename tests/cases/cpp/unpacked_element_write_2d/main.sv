module Top;
  int a [2][3];
  int p00, p01, p02, p10, p11, p12;
  initial begin
    int i, j;
    for (i = 0; i < 2; i = i + 1) begin
      for (j = 0; j < 3; j = j + 1) begin
        a[i][j] = (i + 1) * 10 + j;
      end
    end
    p00 = a[0][0];
    p01 = a[0][1];
    p02 = a[0][2];
    p10 = a[1][0];
    p11 = a[1][1];
    p12 = a[1][2];
  end
endmodule
