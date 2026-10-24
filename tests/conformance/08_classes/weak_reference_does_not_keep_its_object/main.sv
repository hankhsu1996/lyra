// A weak reference names an object without keeping it (LRM 8.30): `get()`
// answers the object it was made with until the reference is cleared, by
// `clear()` or by the memory manager once nothing else refers to the object,
// and null after (LRM 8.30.3, 8.30.4). A null referent is accepted (LRM
// 8.30.2), and two weak references to one object are two objects that answer
// the same referent. `get_id()` answers 0 for null and one nonzero number for
// one object, whichever class of its lineage the handle names it as (LRM
// 8.30.5). The class is an ordinary one, which a class may extend.
//
// `get()` in a wait detects the clearing (LRM 8.30.3). The standard leaves when
// the memory manager clears to the tool, so each wait here has no deadline and
// nothing else to run: only a tool that never reclaims an object nothing refers
// to fails it. A wait that also reads a property of the referent watches that
// property, and watching does not refer to the object (LRM 8.29), so dropping
// the last handle still lets it end. Two referents expiring at once end a
// wait on either, and a wait given up before its referent expires leaves
// nothing behind to be told. A block's own variable refers to an object only
// until the block ends (LRM 6.21), whichever way the block is left -- at its
// end, by `break`, `continue`, `return` or `disable`, or by the process running
// it being killed -- and a wait refers to nothing it read once it has ended, the
// body it controls running after it.
//
// A process is an object a handle names too (LRM 9.7): one that has finished
// is still referred to while a handle names it.
class C;
  int n;
  function new(int n);
    this.n = n;
  endfunction
endclass

class D extends C;
  function new(int n);
    super.new(n);
  endfunction
endclass

class Holder;
  std::weak_reference #(C) kept;
endclass

class Labeled extends std::weak_reference #(C);
  string label;
  function new(C referent, string label);
    super.new(referent);
    this.label = label;
  endfunction
endclass

module Top;
  typedef std::weak_reference #(C) c_ref;
  typedef std::weak_reference #(D) d_ref;

  int checks_done;

  function automatic C referent_of(c_ref w);
    return w.get();
  endfunction

  function automatic void hold_and_return(C c);
    automatic C held = c;
    if (held != null) return;
    $fatal(1, "a function was handed no object");
  endfunction

  // Kept, read and cleared.
  initial begin
    automatic C c = new(7);
    automatic c_ref w = new(c);
    automatic c_ref other = new(c);
    automatic c_ref none = new(null);
    automatic C got;

    got = w.get();
    if (got !== c) $fatal(1, "get() answered another object than its referent");
    if (got.n !== 7) $fatal(1, "the referent read back n=%0d, expected 7", got.n);
    if (w === other) $fatal(1, "two weak references to one object are one object");
    if (w.get() !== other.get())
      $fatal(1, "two weak references to one object answer different referents");
    if (none.get() !== null) $fatal(1, "a weak reference made with null answers an object");

    w.clear();
    if (w.get() !== null) $fatal(1, "a cleared weak reference answers an object");
    if (other.get() !== c) $fatal(1, "clearing one weak reference cleared another");
    checks_done++;
  end

  // Identity.
  initial begin
    automatic D d = new(3);
    automatic C as_base = d;
    automatic C other = new(3);
    automatic longint id;

    id = c_ref::get_id(d);
    if (c_ref::get_id(null) !== 0) $fatal(1, "get_id(null) was not 0");
    if (id === 0) $fatal(1, "get_id of an object was 0");
    if (c_ref::get_id(as_base) !== id)
      $fatal(1, "get_id of one object differs through a base handle");
    if (d_ref::get_id(d) !== id)
      $fatal(1, "get_id of one object differs through another specialization");
    if (c_ref::get_id(d) !== id) $fatal(1, "get_id of one object changed between calls");
    if (c_ref::get_id(other) === id) $fatal(1, "two live objects share an id");
    checks_done++;
  end

  // Held in another object's property and handed to a function.
  initial begin
    automatic C c = new(8);
    automatic Holder h = new;
    h.kept = new(c);
    if (referent_of(h.kept) !== c) $fatal(1, "a weak reference passed on answered another referent");
    c = null;
    #1;
    if (referent_of(h.kept) !== null)
      $fatal(1, "a weak reference held in an object kept its referent");
    checks_done++;
  end

  // A block's variable, and the body of a wait that has ended.
  initial begin
    automatic C c = new(10);
    automatic c_ref w = new(c);
    begin
      automatic C t = c;
      t.n = 11;
    end
    c = null;
    #1;
    if (w.get() !== null) $fatal(1, "a block's variable kept its object past the block");
    c = new(12);
    w = new(c);
    wait (w.get() != null) begin
      c = null;
      #1;
      if (w.get() !== null) $fatal(1, "a wait that had ended kept what it read");
    end
    checks_done++;
  end

  // Every way out of a block.
  initial begin
    automatic C c;
    automatic c_ref w;
    automatic process p;

    for (int i = 0; i < 2; i++) begin
      automatic C t = new(i);
      if (i == 1 && w.get() !== null)
        $fatal(1, "a loop body's variable kept its object into the next iteration");
      w = new(t);
    end
    if (w.get() !== null) $fatal(1, "a loop body's variable kept its object past the loop");

    for (int i = 0; i < 2; i++) begin
      automatic C t = new(i);
      w = new(t);
      break;
    end
    if (w.get() !== null) $fatal(1, "a variable left by break kept its object");

    for (int i = 0; i < 1; i++) begin
      automatic C t = new(i);
      w = new(t);
      continue;
    end
    if (w.get() !== null) $fatal(1, "a variable left by continue kept its object");

    c = new(20);
    w = new(c);
    hold_and_return(c);
    c = null;
    if (w.get() !== null) $fatal(1, "a function's variable kept its object past its return");

    begin : left_by_disable
      automatic C t = new(21);
      w = new(t);
      disable left_by_disable;
    end
    if (w.get() !== null) $fatal(1, "a variable left by disable kept its object");

    c = new(22);
    w = new(c);
    fork
      begin
        automatic C t = c;
        p = process::self();
        #100;
      end
    join_none
    #1;
    p.kill();
    c = null;
    #1;
    if (w.get() !== null) $fatal(1, "the variable of a killed process kept its object");
    checks_done++;
  end

  // An event control on what it answers.
  initial begin
    automatic C c = new(9);
    automatic c_ref w = new(c);
    fork
      #5 c = null;
    join_none
    @(w.get());
    if (w.get() !== null) $fatal(1, "the event control ended with the referent still there");
    checks_done++;
  end

  // A class extending it.
  initial begin
    automatic C c = new(11);
    automatic Labeled l = new(c, "first");
    automatic c_ref as_base = l;
    if (l.label != "first") $fatal(1, "the extending class lost its own property");
    if (l.get() !== c) $fatal(1, "the extending class answered another referent");
    if (as_base.get() !== c) $fatal(1, "the base view answered another referent");
    l.clear();
    if (as_base.get() !== null) $fatal(1, "clearing through the extending class did not clear");
    checks_done++;
  end

  // A wait woken by clear().
  initial begin
    automatic C c = new(1);
    automatic c_ref w = new(c);
    fork
      #5 w.clear();
    join_none
    wait (w.get() == null);
    if ($time !== 5) $fatal(1, "a wait on a cleared reference ended at %0t, expected 5", $time);
    if (c.n !== 1) $fatal(1, "the referent changed");
    checks_done++;
  end

  // A wait woken by the memory manager.
  initial begin
    automatic C c = new(2);
    automatic c_ref w = new(c);
    fork
      #5 c = null;
    join_none
    wait (w.get() == null);
    if ($time < 5) $fatal(1, "a referent was cleared at %0t while still referred to", $time);
    checks_done++;
  end

  // A wait that also reads a property of the referent.
  initial begin
    automatic C c = new(5);
    automatic c_ref w = new(c);
    fork
      #5 c = null;
    join_none
    wait (w.get() == null || w.get().n == 10);
    if (w.get() !== null) $fatal(1, "the wait ended with the referent still there");
    checks_done++;
  end

  // Two referents expiring together.
  initial begin
    automatic C a = new(3);
    automatic C b = new(4);
    automatic c_ref wa = new(a);
    automatic c_ref wb = new(b);
    fork
      #5 begin
        a = null;
        b = null;
      end
    join_none
    wait (wa.get() == null || wb.get() == null);
    if (wa.get() !== null || wb.get() !== null)
      $fatal(1, "a referent outlived the last handle naming it");
    checks_done++;
  end

  // A wait given up before its referent expires.
  initial begin
    automatic C c = new(6);
    automatic c_ref w = new(c);
    fork
      wait (w.get() == null);
      #1;
    join_any
    disable fork;
    c = null;
    #1;
    if (w.get() !== null) $fatal(1, "the referent outlived the last handle naming it");
    checks_done++;
  end

  // A finished process is still referred to while a handle names it.
  initial begin
    automatic process p;
    automatic std::weak_reference #(process) wp;
    fork
      p = process::self();
    join
    wp = new(p);
    if (p.status() !== process::FINISHED) $fatal(1, "the forked process had not finished");
    if (wp.get() !== p) $fatal(1, "a finished process was cleared while a handle names it");
    p = null;
    wait (wp.get() == null);
    checks_done++;
  end

  final begin
    if (checks_done !== 13) $fatal(1, "%0d of 13 groups of checks ran", checks_done);
    $display("All checks passed");
  end
endmodule
