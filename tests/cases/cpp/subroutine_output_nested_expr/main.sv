// A function with an output / inout argument called in a NESTED expression
// position (LRM 13.4): the call is an operand, not the whole statement, but the
// enclosing expression is within a procedural statement, so it is legal SV
// (illegal only in an event expression, a procedural continuous assignment, or
// an expression outside any procedural statement). The output rides the
// completion payload; the call lowers to an immediately-invoked closure that
// writes the output back and yields the function result. This exercises the
// nested forms: an arithmetic operand, a nested call argument, an if condition,
// and an inout.
function automatic int make(input int a, output int o);
  o = a * 10;
  return a + 1;
endfunction

function automatic int wrap(input int x);
  return x * 2;
endfunction

function automatic int bump(inout int v);
  v = v + 1;
  return v * 10;
endfunction

module Test;
  int r_arith;
  int o1;
  int r_nested;
  int o2;
  int r_cond;
  int o3;
  int r_inout;
  int v;

  initial begin
    r_arith = make(3, o1) + 100;
    r_nested = wrap(make(5, o2));
    if (make(7, o3) > 0) r_cond = 1;
    else r_cond = 2;
    v = 5;
    r_inout = bump(v) + 1000;
  end
endmodule
