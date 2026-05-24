module Top;
  initial begin
    int count = 0;
    repeat (3) begin
      repeat (2) begin
        count = count + 1;
      end
    end
    $display("count=%0d", count);
  end
endmodule
