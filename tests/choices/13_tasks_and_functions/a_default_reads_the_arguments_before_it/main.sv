// A default is evaluated in the scope containing the subroutine declaration
// (LRM 13.5.3), which holds no formals, so what a default naming an earlier
// formal means the standard does not say, and the order a call's arguments are
// evaluated in it leaves open (LRM 13.5). The front end binds such a default in
// the subroutine's own scope. Lyra reads the formal it names as the value this
// call hands that formal, never as the formal's storage: for a static
// subroutine that storage still holds the previous call's value (LRM 13.4.2).
// What the call hands an output formal is its type's starting value, an inout
// formal the actual's value, and a ref formal the actual itself (LRM 13.3.2,
// 13.5.2). Arguments are taken left to right, so a default may read one an
// earlier default gave, and an argument the call writes is evaluated once
// however many defaults read it. A constructor's defaults are evaluated on the
// object once it exists and before any constructor runs, so a property they
// read holds what it holds before initialization, which LRM 8.7 leaves
// undefined; Lyra gives it its type's starting value. This case holds Lyra to
// all of that, for every kind of subroutine.
package Q;
  function static int ps(int a, int b = a + 1);
    return a + b;
  endfunction
  function automatic int pa(int a, int b = a * 2);
    return a + b;
  endfunction
endpackage

module M;
  int offset = 100;
  int taken = 0;

  function static int fs(int a, int b = a + 1);
    return a + b;
  endfunction

  function automatic int fa(int a, int b = a + offset, int c = b * 2);
    return a + b + c;
  endfunction

  function automatic int next();
    taken++;
    return taken;
  endfunction

  task automatic tt(input int a, output int o, input int b = a + 3);
    o = b;
  endtask

  function automatic int after_output(output int a, input int b = a);
    a = 5;
    return b;
  endfunction

  function automatic int after_inout(inout int a, input int b = a + 1);
    a = 0;
    return b;
  endfunction

  function automatic int after_ref(ref int a, input int b = a);
    return b;
  endfunction

  // A default of an `output` naming an earlier input binds what the call holds
  // for that input, which is gone with the call.
  task automatic into_earlier(input int a, output int b = a);
    b = a + 1;
  endtask
endmodule

class K;
  int size = 8;
  int got;

  function new(int a = 3, int b = a + size);
    got = b;
  endfunction

  virtual function int m(int a, int b = a + size);
    return b;
  endfunction

  static function int sm(int a, int b = a - 1);
    return b;
  endfunction
endclass

module Top;
  M inst ();

  initial begin
    int r;
    int v;
    K k;

    if (inst.fs(1) !== 3) $fatal(1, "fs(1) gave %0d, expected 3", inst.fs(1));
    if (inst.fs(5) !== 11)
      $fatal(1, "fs(5) gave %0d, expected 11", inst.fs(5));
    if (inst.fs(9) !== 19)
      $fatal(1, "fs(9) gave %0d, expected 19", inst.fs(9));
    if (Q::ps(4) !== 9) $fatal(1, "Q::ps(4) gave %0d, expected 9", Q::ps(4));
    if (Q::ps(6) !== 13)
      $fatal(1, "Q::ps(6) gave %0d, expected 13", Q::ps(6));
    if (Q::pa(5) !== 15)
      $fatal(1, "Q::pa(5) gave %0d, expected 15", Q::pa(5));

    if (inst.fa(1) !== 304)
      $fatal(1, "a chained default gave %0d, expected 304", inst.fa(1));
    r = inst.fa(inst.next());
    if (r !== 304) $fatal(1, "fa(next()) gave %0d, expected 304", r);
    if (inst.taken !== 1)
      $fatal(1, "a written argument was evaluated %0d times, expected 1",
             inst.taken);

    v = -1;
    inst.tt(4, v);
    if (v !== 7) $fatal(1, "a task's default gave %0d, expected 7", v);

    v = 9;
    r = inst.after_output(v);
    if (r !== 0)
      $fatal(1, "a default reading an output formal gave %0d, expected 0", r);
    v = 10;
    r = inst.after_inout(v);
    if (r !== 11)
      $fatal(1, "a default reading an inout formal gave %0d, expected 11", r);
    v = 4;
    r = inst.after_ref(v);
    if (r !== 4)
      $fatal(1, "a default reading a ref formal gave %0d, expected 4", r);

    inst.into_earlier(3);

    k = new;
    if (k.got !== 3)
      $fatal(1, "a constructor's default gave %0d, expected 3", k.got);
    v = inst.taken;
    k = new(inst.next());
    if (inst.taken !== v + 1)
      $fatal(1, "a written constructor argument ran %0d times, expected 1",
             inst.taken - v);
    if (k.got !== v + 1)
      $fatal(1, "a default reading a written argument gave %0d", k.got);
    if (k.m(2) !== 10) $fatal(1, "a method's default gave %0d", k.m(2));
    if (K::sm(7) !== 6)
      $fatal(1, "a static method's default gave %0d", K::sm(7));
    $display("All checks passed");
  end
endmodule
