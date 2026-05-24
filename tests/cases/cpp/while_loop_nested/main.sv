module Top;
  int total;
  initial begin
    int i;
    i = 0;
    total = 0;
    while (i < 3) begin
      int j;
      j = 0;
      while (j < 4) begin
        total = total + 1;
        j = j + 1;
      end
      i = i + 1;
    end
  end
endmodule
