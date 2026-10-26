// randomize() through a handle of an interface class (LRM 8.26.9) randomizes
// the object it refers to and runs that object's hooks, its own or ones it
// inherits (LRM 18.6.2). That holds through an interface class that extends
// another, with an inline constraint on a state variable, and where the
// interface class and the base are declared in another unit. The standard
// grants no other method of an object through such a handle, and does not
// settle which hook a call naming one runs, so this case calls neither.
package p;
  interface class I;
  endclass

  class B;
    int log = 0;
    function void post_randomize();
      log = log * 10 + 3;
    endfunction
  endclass
endpackage

interface class J extends p::I;
endclass

class Own implements J;
  rand bit [3:0] v;
  int log = 0;
  constraint c { v == 9; }

  function void pre_randomize();
    log = log * 10 + 1;
  endfunction

  function void post_randomize();
    log = log * 10 + 2;
  endfunction
endclass

// Declares neither hook; inherits B's post_randomize.
class Inherits extends p::B implements p::I;
  rand bit [15:0] w;
endclass

module Top;
  int lim = 3;
  int randomized = -1;
  int own_v = -1;
  int own_randomize = -1;
  int with_state = -1;
  int inherited_hook = -1;

  initial begin
    Own o;
    Inherits n;
    p::I i;
    J j;

    o = new;
    n = new;
    i = o;
    randomized = i.randomize();
    own_v = o.v;
    own_randomize = o.log;
    j = o;
    o.log = 0;
    with_state = j.randomize() with { lim == 3; };
    with_state = with_state * 100 + o.log;

    i = n;
    void'(i.randomize());
    inherited_hook = n.log;
  end

  final begin
    if (randomized !== 1)
      $fatal(1, "randomize() through an interface class handle answered %0d",
             randomized);
    if (own_v !== 9)
      $fatal(1, "randomize() through an interface class handle left v %0d, expected 9",
             own_v);
    if (own_randomize !== 12)
      $fatal(1, "randomize() through an interface class handle ran hooks %0d, expected 12",
             own_randomize);
    if (with_state !== 112)
      $fatal(1, "randomize() with a state constraint left %0d, expected 112",
             with_state);
    if (inherited_hook !== 3)
      $fatal(1, "randomize() through an interface class handle ran the inherited hook to %0d, expected 3",
             inherited_hook);
    $display("All checks passed");
  end
endmodule
