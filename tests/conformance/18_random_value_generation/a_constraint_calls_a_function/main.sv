// A constraint may call a function, whose answer is a value the constraint
// reads (LRM 18.5.11). A method named without a handle is the method of the
// object whose constraint it is: in a class's own block, the object being
// randomized, and in the constraints a `randomize() with` call writes, also
// the object being randomized wherever its class declares the name, since a
// name there is looked up in that class first (LRM 18.7). A name the class
// does not declare is looked up where the call is written, so a method of the
// class whose method makes the call is that object's.
class Target;
  rand bit [7:0] v;
  rand bit [7:0] w;
  int bound = 3;

  function int limit();
    return bound;
  endfunction

  constraint own { w == limit() + 1; }
endclass

class Caller;
  int offset = 20;

  function int base();
    return offset;
  endfunction

  function int pin(Target t);
    return t.randomize() with { v == limit() + base(); };
  endfunction
endclass

module Top;
  int named = -1;
  int called = -1;
  bit [7:0] v_named = 0;
  bit [7:0] w_named = 0;
  bit [7:0] v_called = 0;

  initial begin
    Target t;
    Caller c;
    t = new;
    c = new;
    t.bound = 9;
    named = t.randomize() with { v == limit(); };
    v_named = t.v;
    w_named = t.w;
    t.bound = 4;
    called = c.pin(t);
    v_called = t.v;
  end

  final begin
    if (named !== 1 || v_named !== 9)
      $fatal(1, "an inline constraint calling the object's method answered %0d, %0d",
             named, v_named);
    if (w_named !== 10)
      $fatal(1, "a class constraint calling the object's method chose %0d", w_named);
    if (called !== 1 || v_called !== 24)
      $fatal(1, "an inline constraint calling both objects' methods answered %0d, %0d",
             called, v_called);
    $display("All checks passed");
  end
endmodule
