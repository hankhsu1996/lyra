// A class declared inside a module or a generate block is a type of that
// instance, so an object of it belongs to the one instance it was created in
// and its body reaches that instance's own declarations -- reading a variable,
// writing one, and calling a function the scope declares (LRM 6.22, 23.9).
// Two instances of one module therefore carry two unrelated sets of objects:
// each reaches its own instance's storage and never the other's, which is what
// the differing seeds below distinguish. A body with no object to reach the
// instance through -- a static method, or a constructor still building its
// object -- reaches it all the same, including from a process it forks
// (LRM 9.3.2), which runs after the body that started it has returned.
module Holder (input int seed);
  int slot = -1;
  int read_back = -1;
  int doubled = -1;
  int static_forked = -1;
  int ctor_forked = -1;

  function automatic int twice(int v);
    return v * 2;
  endfunction

  class Spawner;
    function new();
      fork
        ctor_forked = seed + 2;
      join_none
    endfunction
  endclass

  class Peeker;
    function int peek();
      return slot;
    endfunction

    function void poke(int v);
      slot = v;
    endfunction

    function int through_scope();
      return twice(slot);
    endfunction

    static function void spawn();
      fork
        static_forked = seed + 1;
      join_none
    endfunction
  endclass

  initial begin
    Peeker p = new();
    Spawner s;
    p.poke(seed);
    read_back = p.peek();
    doubled = p.through_scope();
    Peeker::spawn();
    s = new();
  end
endmodule

module Top;
  Holder u1 (.seed(10));
  Holder u2 (.seed(21));

  if (1) begin : g
    int owned = 3;

    class Toucher;
      function void bump();
        owned = owned + 4;
      endfunction
    endclass

    initial begin
      Toucher t = new();
      t.bump();
    end
  end

  final begin
    if (u1.slot !== 10) $fatal(1, "u1.slot was %0d, expected 10", u1.slot);
    if (u2.slot !== 21) $fatal(1, "u2.slot was %0d, expected 21", u2.slot);
    if (u1.read_back !== 10)
      $fatal(1, "u1.read_back was %0d, expected 10", u1.read_back);
    if (u2.read_back !== 21)
      $fatal(1, "u2.read_back was %0d, expected 21", u2.read_back);
    if (u1.doubled !== 20)
      $fatal(1, "u1.doubled was %0d, expected 20", u1.doubled);
    if (u2.doubled !== 42)
      $fatal(1, "u2.doubled was %0d, expected 42", u2.doubled);
    if (u1.static_forked !== 11)
      $fatal(1, "u1.static_forked was %0d, expected 11", u1.static_forked);
    if (u2.static_forked !== 22)
      $fatal(1, "u2.static_forked was %0d, expected 22", u2.static_forked);
    if (u1.ctor_forked !== 12)
      $fatal(1, "u1.ctor_forked was %0d, expected 12", u1.ctor_forked);
    if (u2.ctor_forked !== 23)
      $fatal(1, "u2.ctor_forked was %0d, expected 23", u2.ctor_forked);
    if (g.owned !== 7) $fatal(1, "g.owned was %0d, expected 7", g.owned);
    $display("All checks passed");
  end
endmodule
