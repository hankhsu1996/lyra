module Top;
  initial begin
    int n = 6;
    int sum = 0;
    int i = 1;
    repeat (n) begin
      sum = sum + i;
      i = i + 1;
    end
    $display("sum=%0d", sum);
  end
endmodule
