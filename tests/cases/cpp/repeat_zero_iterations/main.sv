module Top;
  int sum;
  initial begin
    sum = 0;
    repeat (0) begin
      sum = sum + 1;
    end
  end
endmodule
