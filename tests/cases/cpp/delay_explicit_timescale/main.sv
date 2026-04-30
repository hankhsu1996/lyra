module Top;
  timeunit 1ns;
  timeprecision 1ps;
  initial begin
    $display("a");
    #5;
    $display("b");
    #5ns;
    $display("c");
  end
endmodule
