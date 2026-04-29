module Top;
  initial #5;

  initial #5 $display("x");

  initial #5ns $display("ns");

  initial begin
    #5 begin
      $display("block");
    end
  end
endmodule
