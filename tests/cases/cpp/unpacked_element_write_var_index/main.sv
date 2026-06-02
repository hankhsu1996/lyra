module Top;
  int a [4];
  int p0, p1, p2, p3;
  initial begin
    int i;
    for (i = 0; i < 4; i = i + 1) begin
      a[i] = i * 100;
    end
    p0 = a[0];
    p1 = a[1];
    p2 = a[2];
    p3 = a[3];
  end
endmodule
