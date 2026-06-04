// $printtimescale prints the time unit and precision of the current scope (LRM
// 20.4.2) in the Table 20-2 spelling.
`timescale 1ns / 1ps
module Top;
  initial $printtimescale;
endmodule
