// A structure of N members, each a string, so building, copying, comparing
// and ending it runs once per member.
// @each-adds: bytes
module Top #(parameter int N = 4);
  typedef struct { [[string m{i};| ]] } s_t;

  s_t a, b;
  bit same;

  initial begin
    a.m{last} = "last";
    b = a;
    b <= a;
    same = a == b;
  end
endmodule
