module Top;
  int count;
  initial begin
    int i;
    i = 0;
    count = 0;
    while (i < 5) begin
      repeat (2) begin
        count = count + 1;
      end
      i = i + 1;
    end
  end
endmodule
