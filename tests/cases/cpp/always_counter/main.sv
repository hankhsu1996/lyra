module Top;
  bit clk;
  int count;

  initial begin
    clk = 0;
    count = 0;
  end

  always @(posedge clk) count = count + 1;

  initial begin
    repeat (5) begin
      #5 clk = 1;
      #5 clk = 0;
    end
    $finish;
  end
endmodule
