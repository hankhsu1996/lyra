// A sum of N operands, each a value too large for a frame: each link's answer
// is made and then read by the next, so each is held off the stack, and the
// chain the source writes flat stays flat whatever its length.
// @each-adds: bytes
module Top #(parameter int N = 4);
  logic [19999:0] a, s;

  initial begin
    a = 1;
    s = a [[+ a|]];
  end
endmodule
