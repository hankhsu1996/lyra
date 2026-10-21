// A structure of N members, each a four-state byte, filled through an
// assignment pattern naming every member, copied, compared both ways, assigned
// by nonblocking assignment, passed to a function and back, and converted to
// its bits: every whole-value operation the language gives a structure runs
// over every member. The value stays inside what a frame keeps at both sizes.
// @each-adds: bytes,
// @each-adds: mir.integral constants 1, lir.integral constants 1
module Top #(parameter int N = 4);
  typedef struct { [[logic [7:0] m{i};| ]] } s_t;
  typedef logic [8*N-1:0] bits_t;

  function automatic s_t passed(s_t v);
    return v;
  endfunction

  s_t a, b, c;
  bits_t bits;
  bit same, identical;

  initial begin
    a = '{[[m{i}: 8'h5|, ]]};
    b = passed(a);
    c <= a;
    same = a == b;
    identical = a === b;
    bits = bits_t'(a);
  end
endmodule
