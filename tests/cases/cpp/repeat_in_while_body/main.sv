module Top;
  initial begin
    int i = 0;
    int count = 0;
    while (i < 5) begin
      repeat (2) begin
        count = count + 1;
      end
      i = i + 1;
    end
    $display("count=%0d", count);
  end
endmodule
