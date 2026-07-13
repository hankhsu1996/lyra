// Class property initializers (LRM 8.7). During construction each property is
// set in declaration order: a property with an explicit `= value` takes that
// value -- and the initializer may read an earlier property through the
// implicit receiver -- while one without takes its type's default. All property
// initialization happens before the user constructor body runs. A class that
// declares no `function new` still initializes its properties through the
// implicit constructor. Products are copied into module variables so the test
// asserts the feature's effect, not a print side effect.
module Top;
  class WithCtor;
    int x = 5;
    int y = x + 1;
    int z;
    function new(int a);
      z = a + y;
    endfunction
  endclass

  class NoCtor;
    int p = 7;
    int q = p + p;
  endclass

  int gx;
  int gy;
  int gz;
  int gp;
  int gq;

  initial begin
    WithCtor w;
    NoCtor n;
    w = new(10);
    n = new;
    gx = w.x;
    gy = w.y;
    gz = w.z;
    gp = n.p;
    gq = n.q;
  end
endmodule
