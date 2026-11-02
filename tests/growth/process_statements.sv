// N statements in a process that never stops, which is an ordinary function.
// @each-adds: bytes,
// @each-adds: mir.integral constants 1, lir.integral constants 1
module Top #(parameter int N = 4);
  bit z;
  int c;

  initial begin
    [[if (z) c = {i};| ]]
  end
endmodule
