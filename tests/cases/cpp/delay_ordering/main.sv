module Top;
  timeunit 1ns;
  timeprecision 1ps;
  initial begin
    $display("a");
    #5ns;
    $display("b");
  end
  initial begin
    $display("c");
  end
endmodule
