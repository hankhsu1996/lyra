module Top;
  logic clk;
  int   done;
  initial begin
    clk = 0;
    done = 0;
    @(clk);
    done = 1;
  end
  initial begin
    #5;
    clk = 1;
  end
endmodule
