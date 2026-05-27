module Top;
  logic clk;
  int   done;
  initial begin
    clk = 1;
    done = 0;
    @(negedge clk);
    done = 1;
  end
  initial begin
    #5;
    clk = 0;
  end
endmodule
