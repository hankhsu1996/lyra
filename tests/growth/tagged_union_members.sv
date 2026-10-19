// A tagged union of N members, the last of which owns storage, each member
// written and read: every member is reached by its position, and copying the
// union follows the member its tag names.
// @each-adds: bytes,
// @each-adds: mir.integral constants 1, lir.integral constants 1
module Top #(parameter int N = 4);
  typedef union tagged { [[int m{i};| ]] string text; } u_t;

  u_t u, w;
  int sum;

  initial begin
    sum = 0;
    [[u = tagged m{i} {i}; w = u; sum = sum + w.m{i};| ]]
    u = tagged text "last";
    w = u;
  end
endmodule
