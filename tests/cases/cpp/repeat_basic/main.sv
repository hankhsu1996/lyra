module Top;
  initial begin
    int sum = 0;
    int i = 0;
    repeat (5) begin
      sum = sum + i;
      i = i + 1;
    end
    $display("sum=%0d", sum);
  end
endmodule
