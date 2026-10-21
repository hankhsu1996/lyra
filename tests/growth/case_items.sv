// A case statement of N items and an item of N labels, and a casez of N items:
// lists the source writes side by side, which a step makes one longer and no
// deeper (LRM 12.5, 12.5.1).
// @each-adds: bytes,
// @each-adds: mir.integral constants 1, lir.integral constants 1
module Top #(parameter int N = 4);
  int s, r;

  initial begin
    s = {last};
    case (s)
[[      {i}: r = {i};|
]]
      default: r = -1;
    endcase
    case (s)
      [[{i}|, ]]: r = r + 1;
    endcase
    casez (s)
[[      {i}: r = r + 2;|
]]
    endcase
  end
endmodule
