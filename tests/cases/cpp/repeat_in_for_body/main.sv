module Top;
  initial begin
    int count = 0;
    for (int i = 0; i < 4; i = i + 1) begin
      repeat (3) begin
        count = count + 1;
      end
    end
    $display("count=%0d", count);
  end
endmodule
