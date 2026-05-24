module Top;
  initial begin
    int sum = 0;
    repeat (0) begin
      sum = sum + 1;
    end
    $display("sum=%0d", sum);
  end
endmodule
