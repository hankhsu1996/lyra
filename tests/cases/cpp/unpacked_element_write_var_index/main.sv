module Top;
  int a [4];
  initial begin
    int i;
    for (i = 0; i < 4; i = i + 1) begin
      a[i] = i * 100;
    end
  end
endmodule
