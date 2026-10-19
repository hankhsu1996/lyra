// An unpacked union of N members, each read: every member is reached by its
// position.
// @each-adds: bytes
module Top #(parameter int N = 4);
  typedef union { [[int m{i};| ]] } u_t;

  u_t u, w;
  int sum;

  initial begin
    u.m{last} = 7;
    w = u;
    sum = 0;
    [[sum = sum + w.m{i};| ]]
  end
endmodule
