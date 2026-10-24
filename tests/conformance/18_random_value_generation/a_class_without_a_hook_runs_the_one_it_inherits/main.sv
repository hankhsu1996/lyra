// Every class has pre_randomize() and post_randomize() (LRM 18.6.2). One that
// declares neither has hooks that invoke its base's, so a call naming them on
// such a class runs the hook the nearest class above declares, and the empty
// one where no class above declares any. That holds however the call names
// them -- through super from an override (LRM 18.3's own example), by plain
// name, through this, through a handle -- whether the declaring class is
// defined in another unit or defines its hook outside its body, whether one or
// two classes in between declare none, and whether the lineage begins at a
// built-in class, which declares none. The hooks are not virtual, so a
// base handle calling one runs the base's, while randomize() through the same
// handle runs the object's. A method every class has is called by plain name
// from another method the same way (LRM 18.13.3, 18.8).
package hooks;
  class Root;
    int log = 0;
    rand bit [3:0] v;
  endclass

  class Base extends Root;
    function void pre_randomize();
      log = log * 10 + 5;
    endfunction

    extern function void post_randomize();
  endclass

  function void Base::post_randomize();
    log = log * 10 + 1;
  endfunction

  class Mid extends Base;
  endclass
endpackage

class Mid2 extends hooks::Mid;
endclass

// Overrides both through two classes that declare neither.
class ThroughMids extends Mid2;
  function void pre_randomize();
    super.pre_randomize();
    log = log * 10 + 3;
  endfunction

  function void post_randomize();
    super.post_randomize();
    log = log * 10 + 2;
  endfunction
endclass

// Overrides both where no class above declares any.
class OverRoot extends hooks::Root;
  function void pre_randomize();
    super.pre_randomize();
    log = 7;
  endfunction

  function void post_randomize();
    super.post_randomize();
    log = log * 10 + 2;
  endfunction
endclass

// Declares neither, and calls them from its own methods.
class Callers extends Mid2;
  function void plain();
    post_randomize();
  endfunction

  function void through_this();
    this.pre_randomize();
  endfunction

  function void through_super();
    super.post_randomize();
  endfunction

  function void modes();
    srandom(3);
    rand_mode(0);
    log = 8;
  endfunction
endclass

class CallsRoot extends hooks::Root;
  function void plain();
    post_randomize();
    log = log * 10 + 3;
  endfunction
endclass

// A built-in class declares no hook either (LRM 15.3, 15.4), so one above a
// class is a lineage with none declared.
class OverSemaphore extends semaphore;
  int log = 4;

  function new();
    super.new(1);
  endfunction

  function void post_randomize();
    log = log * 10 + 2;
  endfunction

  function void plain();
    pre_randomize();
  endfunction
endclass

class FromMailbox extends mailbox #(int);
  int log = 6;
endclass

module Top;
  int through_mids = -1;
  int over_root = -1;
  int plain = -1;
  int through_this = -1;
  int through_super = -1;
  int modes = -1;
  int v_mode = -1;
  int root_plain = -1;
  int over_semaphore = -1;
  int from_mailbox = -1;
  int by_handle = -1;
  int root_by_handle = -1;
  int base_handle_hook = -1;
  int base_handle_randomize = -1;

  initial begin
    ThroughMids t;
    OverRoot o;
    Callers c;
    CallsRoot cr;
    OverSemaphore os;
    FromMailbox fm;
    Mid2 m;
    hooks::Root r;
    hooks::Base h;

    t = new;
    if (t.randomize() == 1) through_mids = t.log;
    o = new;
    if (o.randomize() == 1) over_root = o.log;

    c = new;
    c.plain();
    plain = c.log;
    c.log = 0;
    c.through_this();
    through_this = c.log;
    c.log = 0;
    c.through_super();
    through_super = c.log;
    c.modes();
    modes = c.log;
    v_mode = c.v.rand_mode();
    cr = new;
    cr.plain();
    root_plain = cr.log;

    os = new;
    os.post_randomize();
    os.plain();
    over_semaphore = os.log;
    fm = new;
    fm.post_randomize();
    from_mailbox = fm.log;

    m = new;
    m.post_randomize();
    by_handle = m.log;
    r = new;
    r.log = 4;
    r.post_randomize();
    root_by_handle = r.log;

    t = new;
    h = t;
    h.post_randomize();
    base_handle_hook = t.log;
    t.log = 0;
    if (h.randomize() == 1) base_handle_randomize = t.log;
  end

  final begin
    // pre: Base's 5, then 3; post: Base's 1, then 2.
    if (through_mids !== 5312)
      $fatal(1, "hooks through two classes declaring none left %0d, expected 5312",
             through_mids);
    if (over_root !== 72)
      $fatal(1, "hooks over the root left %0d, expected 72", over_root);
    if (plain !== 1)
      $fatal(1, "a plain call of an inherited hook left %0d, expected 1", plain);
    if (through_this !== 5)
      $fatal(1, "this.pre_randomize() left %0d, expected 5", through_this);
    if (through_super !== 1)
      $fatal(1, "super.post_randomize() from a method left %0d, expected 1",
             through_super);
    if (modes !== 8)
      $fatal(1, "srandom and rand_mode by plain name left %0d, expected 8", modes);
    if (v_mode !== 0)
      $fatal(1, "rand_mode(0) by plain name left v's mode %0d, expected 0", v_mode);
    if (root_plain !== 3)
      $fatal(1, "a plain call of the root hook left %0d, expected 3", root_plain);
    if (over_semaphore !== 42)
      $fatal(1, "hooks over a semaphore left %0d, expected 42", over_semaphore);
    if (from_mailbox !== 6)
      $fatal(1, "a hook of a mailbox's subclass left %0d, expected 6", from_mailbox);
    if (by_handle !== 1)
      $fatal(1, "a handle call of an inherited hook left %0d, expected 1", by_handle);
    if (root_by_handle !== 4)
      $fatal(1, "a handle call of the root hook left %0d, expected 4", root_by_handle);
    if (base_handle_hook !== 1)
      $fatal(1, "a base handle's hook call left %0d, expected the base's 1",
             base_handle_hook);
    if (base_handle_randomize !== 5312)
      $fatal(1, "randomize() through a base handle left %0d, expected 5312",
             base_handle_randomize);
    $display("All checks passed");
  end
endmodule
