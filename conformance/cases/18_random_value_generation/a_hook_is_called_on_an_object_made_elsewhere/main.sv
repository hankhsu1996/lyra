// Every class has pre_randomize() and post_randomize() (LRM 18.6.2), so a call
// of either through a handle is legal whoever made the object the handle
// holds. A class declaring neither invokes the one the nearest class above it
// declares, and the empty built-in one where none above declares any. Here
// every object is made by a function declared beside its class, and the module
// calling the hooks only holds handles to them: of a class declaring none and
// extending nothing, whose hooks change nothing; of a class declaring none
// whose base declares both, which run the base's; and of an interface class
// the object's class implements. For the last, LRM 8.26.9 gives an interface
// class its own empty virtual hooks, which the implementing class's override,
// while 18.6.2 says the hooks are not virtual, so the call may run either the
// empty one or the object's; both leave the run going.
class Plain;
  int log = 0;
endclass

function automatic Plain make_plain();
  Plain p = new;
  p.log = 4;
  return p;
endfunction

function automatic int plain_log(Plain p);
  return p.log;
endfunction

package made;
  class Base;
    int log = 0;

    function void pre_randomize();
      log = log * 10 + 5;
    endfunction

    function void post_randomize();
      log = log * 10 + 1;
    endfunction
  endclass

  class Derived extends Base;
  endclass

  interface class Hooked;
    pure virtual function int id();
  endclass

  class Implements implements Hooked;
    int log = 4;

    virtual function int id();
      return 1;
    endfunction

    function void post_randomize();
      log = log * 10 + 2;
    endfunction
  endclass

  Derived derived;
  Implements implementing;

  function automatic Derived make_derived();
    derived = new;
    return derived;
  endfunction

  function automatic Hooked make_hooked();
    implementing = new;
    return implementing;
  endfunction

  function automatic int derived_log();
    return derived.log;
  endfunction

  function automatic int implements_log();
    return implementing.log;
  endfunction
endpackage

module Top;
  int plain = -1;
  int derived = -1;
  int hooked = -1;
  bit hooked_returned = 0;

  initial begin
    Plain p;
    made::Derived d;
    made::Hooked h;

    p = make_plain();
    p.pre_randomize();
    p.post_randomize();
    plain = plain_log(p);

    d = made::make_derived();
    d.pre_randomize();
    d.post_randomize();
    derived = made::derived_log();

    h = made::make_hooked();
    h.pre_randomize();
    h.post_randomize();
    hooked_returned = 1;
    hooked = made::implements_log();
  end

  final begin
    if (plain !== 4)
      $fatal(1, "the hooks of a class declaring none left %0d, expected 4",
             plain);
    // pre: Base's 5; post: Base's 1.
    if (derived !== 51)
      $fatal(1, "the hooks a base declares left %0d, expected 51", derived);
    if (hooked_returned !== 1)
      $fatal(1, "the hooks through an interface class handle did not return");
    if (hooked !== 4 && hooked !== 42)
      $fatal(1, "hooks through an interface handle left %0d, expected 4 or 42",
             hooked);
    $display("All checks passed");
  end
endmodule
