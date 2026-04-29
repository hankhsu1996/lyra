module Top;
  timeunit 1ns;
  timeprecision 1ps;
  initial begin #5ns; $display("a"); end
  initial begin #5ns; $display("b"); end
  initial begin #5ns; $display("c"); end
endmodule
