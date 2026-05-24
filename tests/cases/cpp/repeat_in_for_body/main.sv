module Top;
  int count;
  initial begin
    count = 0;
    for (int i = 0; i < 4; i = i + 1) begin
      repeat (3) begin
        count = count + 1;
      end
    end
  end
endmodule
