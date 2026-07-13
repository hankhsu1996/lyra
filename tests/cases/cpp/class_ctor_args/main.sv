// A user-defined constructor (LRM 8.7): `function new` with input formals and a
// body, invoked as `new(actual_args)`. The constructor runs the body after
// each property takes its default, and the arguments flow to the formals.
// Products are copied into module variables so the test asserts the feature's
// effect, not a print side effect.
module Top;
  class Point;
    int x;
    int y;
    function new(int a, int b);
      x = a;
      y = b + a;
    endfunction
  endclass

  int gx;
  int gy;

  initial begin
    Point p;
    p = new(3, 4);
    gx = p.x;
    gy = p.y;
  end
endmodule
