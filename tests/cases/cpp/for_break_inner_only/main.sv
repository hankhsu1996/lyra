module Top;
  int count;
  initial begin
    count = 0;
    for (int i = 0; i < 2; i = i + 1) begin
      int j = 0;
      while (j < 3) begin
        if (j == 1) break;
        count = count + 1;
        j = j + 1;
      end
    end
  end
endmodule
