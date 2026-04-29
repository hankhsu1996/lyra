module Top;
  timeunit 1ns;
  timeprecision 1ps;
  initial begin #10ns; $display("late"); end
  initial begin #5ns; $display("early"); end
endmodule
