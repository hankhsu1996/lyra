// A handle of an interface class reaches the methods every class object has
// (LRM 8.26.9): randomize() randomizes the object it refers to and runs that
// object's hooks, srandom, get_randstate and set_randstate act on that object's
// generator (LRM 18.13.3 -- 18.13.5), and the two hooks an interface class
// contains are virtual, so calling one through the handle runs the hook the
// object's own class runs -- its own, one it inherits, or the empty one where
// no class of its lineage declares any. That holds through an interface class
// that extends another, with an inline constraint on a state variable, and
// where the interface class and the base are declared in another unit.
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

// No class of its lineage declares a hook.
class None implements J;
  int log = 7;
endclass

module Top;
  int lim = 3;
  int randomized = -1;
  int own_v = -1;
  int own_randomize = -1;
  int with_state = -1;
  int own_hooks = -1;
  int inherits_hook = -1;
  int none_hooks = -1;
  int same_state = -1;
  int reseeded = -1;

  initial begin
    Own o;
    Inherits n;
    None z;
    p::I i;
    J j;
    string state;
    bit [15:0] first, again, after_seed;

    o = new;
    n = new;
    z = new;
    i = o;
    randomized = i.randomize();
    own_v = o.v;
    own_randomize = o.log;
    j = o;
    o.log = 0;
    with_state = j.randomize() with { lim == 3; };
    with_state = with_state * 100 + o.log;

    o.log = 0;
    j.pre_randomize();
    i.post_randomize();
    own_hooks = o.log;

    i = n;
    i.post_randomize();
    inherits_hook = n.log;

    j = z;
    j.pre_randomize();
    j.post_randomize();
    none_hooks = z.log;

    i = n;
    i.srandom(5);
    state = i.get_randstate();
    void'(n.randomize());
    first = n.w;
    i.set_randstate(state);
    void'(i.randomize());
    again = n.w;
    same_state = first == again;
    i.srandom(5);
    void'(n.randomize());
    after_seed = n.w;
    reseeded = after_seed == first;
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
    if (own_hooks !== 12)
      $fatal(1, "the hooks of a class declaring both left %0d, expected 12",
             own_hooks);
    if (inherits_hook !== 3)
      $fatal(1, "post_randomize of a class inheriting it left %0d, expected 3",
             inherits_hook);
    if (none_hooks !== 7)
      $fatal(1, "the hooks of a lineage declaring none left %0d, expected 7",
             none_hooks);
    if (same_state !== 1)
      $fatal(1, "set_randstate through an interface class handle did not restore");
    if (reseeded !== 1)
      $fatal(1, "srandom through an interface class handle did not reseed");
    $display("All checks passed");
  end
endmodule
