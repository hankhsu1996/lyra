module Top;
  int count;
  initial begin
    count = 0;
    repeat (3) begin
      repeat (2) begin
        count = count + 1;
      end
    end
  end
endmodule
